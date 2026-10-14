/**
 * Client-data masking. Every prompt to Claude passes through `maskForLlm` in
 * src/lib/llm.ts. That is the ONLY place the app talks to the model (enforced by
 * tests/llm-gateway.test.ts), so nothing reaches Claude unmasked.
 *
 * Pattern-based by design: deterministic, fast, no data leaves the process.
 * It cannot find names or free-form addresses; see docs/04-wisdom-assistant.md.
 *
 * Tuning (env):
 *   REDACT_ALLOW_EMAIL_DOMAINS  comma list of internal domains left readable (e.g. "acme.com")
 *   REDACT_EXTRA_PATTERNS       JSON list: [{"name":"CLIENT_ID","pattern":"\\bCL\\d{8}\\b","flags":"i"}]
 */

export type RedactionCounts = Record<string, number>;

export interface RedactOptions {
  /** Mask email addresses (except allow-listed domains). Default true. */
  emails?: boolean;
  /** Internal email domains left readable. Defaults to REDACT_ALLOW_EMAIL_DOMAINS. */
  allowEmailDomains?: string[];
  /** Extra custom patterns. Defaults to REDACT_EXTRA_PATTERNS. */
  extraPatterns?: { name: string; pattern: string; flags?: string }[];
}

interface Rule {
  name: string;
  re: RegExp;
  replace: (m: string, ...groups: string[]) => string;
}

const last4 = (s: string) => s.replace(/\D/g, "").slice(-4);

/** Luhn check: real card numbers pass, most random digit runs don't. */
export function luhn(digits: string): boolean {
  let sum = 0;
  let dbl = false;
  for (let i = digits.length - 1; i >= 0; i--) {
    let d = digits.charCodeAt(i) - 48;
    if (d < 0 || d > 9) return false;
    if (dbl) {
      d *= 2;
      if (d > 9) d -= 9;
    }
    sum += d;
    dbl = !dbl;
  }
  return digits.length >= 13 && sum % 10 === 0;
}

// Order matters: specific, high-confidence rules first.
const BUILTIN: Rule[] = [
  {
    name: "PRIVATE_KEY",
    re: /-----BEGIN [A-Z ]*PRIVATE KEY-----[\s\S]*?-----END [A-Z ]*PRIVATE KEY-----/g,
    replace: () => "[REDACTED-PRIVATE-KEY]",
  },
  {
    name: "JWT",
    re: /\beyJ[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}\b/g,
    replace: () => "[REDACTED-JWT]",
  },
  { name: "AWS_KEY", re: /\b(?:AKIA|ASIA)[A-Z0-9]{16}\b/g, replace: () => "[REDACTED-AWS-KEY]" },
  {
    name: "SECRET",
    re: /\b(bearer|token|api[_-]?key|apikey|password|passwd|pwd|secret|client[_-]?secret)(["':=\s]+)([A-Za-z0-9._~+/=-]{8,})/gi,
    replace: (_m, k: string, sep: string) => `${k}${sep}[REDACTED-SECRET]`,
  },
  {
    // 123-45-6789 / 123 45 6789
    name: "SSN",
    re: /\b(?!000|666)\d{3}[- ](?!00)\d{2}[- ](?!0000)\d{4}\b/g, // includes 9xx: ITINs
    replace: () => "[REDACTED-SSN]",
  },
  {
    // 9 bare digits next to an SSN/TIN keyword
    name: "SSN",
    re: /\b(ssn|social security(?: number| no\.?)?|tax ?id|tin|itin)(\W{0,6})(\d{9})\b/gi,
    replace: (_m, k: string, sep: string) => `${k}${sep}[REDACTED-SSN]`,
  },
  {
    name: "EIN",
    re: /\b(ein|employer id(?:entification)?(?: number)?)(\W{0,6})(\d{2}-?\d{7})\b/gi,
    replace: (_m, k: string, sep: string) => `${k}${sep}[REDACTED-EIN]`,
  },
  {
    name: "DOB",
    re: /\b(dob|d\.o\.b\.|date of birth|birth ?date|birthday)(\W{0,6})(\d{1,4}[-/.]\d{1,2}[-/.]\d{1,4})/gi,
    replace: (_m, k: string, sep: string) => `${k}${sep}[REDACTED-DOB]`,
  },
  {
    // card numbers: 13-19 digits, optional space/dash groups, Luhn-valid
    name: "CARD",
    re: /\b\d(?:[ -]?\d){12,18}\b/g,
    replace: (m) => (luhn(m.replace(/\D/g, "")) ? `[REDACTED-CARD …${last4(m)}]` : m),
  },
  {
    // account / routing numbers named as such (any length 5-20)
    name: "ACCOUNT",
    re: /\b(acct|account|a\/c|account no\.?|account number|acc no\.?|routing|aba|iban)(\W{0,6})([A-Z]{0,4}[\d][\d -]{3,22}\d)\b/gi,
    replace: (_m, k: string, sep: string, n: string) => `${k}${sep}[REDACTED-ACCT …${last4(n)}]`,
  },
  {
    // any other long digit run (10+): account-like ids in logs
    name: "LONG_NUMBER",
    re: /\b\d{10,}\b/g,
    replace: (m) => `[REDACTED-NUM …${last4(m)}]`,
  },
  {
    name: "PHONE",
    re: /(?<![\w-])(?:\+?1[ .-]?)?\(?\b\d{3}\)?[ .-]\d{3}[ .-]\d{4}\b/g,
    replace: () => "[REDACTED-PHONE]",
  },
];

const EMAIL = /\b[A-Za-z0-9._%+-]+@([A-Za-z0-9.-]+\.[A-Za-z]{2,})\b/g;

function envList(name: string): string[] {
  return (process.env[name] ?? "")
    .split(",")
    .map((s) => s.trim().toLowerCase())
    .filter(Boolean);
}

let extraCache: { raw: string; rules: Rule[] } | null = null;
function extraRules(override?: RedactOptions["extraPatterns"]): Rule[] {
  const make = (list: { name: string; pattern: string; flags?: string }[]): Rule[] =>
    list.map((p) => {
      const flags = Array.from(new Set(((p.flags ?? "") + "g").split(""))).join("");
      const name = p.name.toUpperCase().replace(/[^A-Z0-9_]/g, "_");
      return { name, re: new RegExp(p.pattern, flags), replace: () => `[REDACTED-${name}]` };
    });
  if (override) return make(override);
  const raw = process.env.REDACT_EXTRA_PATTERNS ?? "";
  if (!raw) return [];
  if (extraCache?.raw === raw) return extraCache.rules;
  try {
    const rules = make(JSON.parse(raw));
    extraCache = { raw, rules };
    return rules;
  } catch (e) {
    // Fail loudly: a broken custom pattern must not silently disable masking.
    throw new Error(`REDACT_EXTRA_PATTERNS is invalid: ${e instanceof Error ? e.message : e}`);
  }
}

/** Mask sensitive values; returns the masked text and how many of each kind were masked. */
export function redactWithCounts(text: string, opts: RedactOptions = {}): { text: string; counts: RedactionCounts } {
  const counts: RedactionCounts = {};
  if (!text) return { text, counts };
  const bump = (n: string) => (counts[n] = (counts[n] ?? 0) + 1);
  let out = text;
  // Custom patterns run first so organisation-specific ids win over generic number rules.
  for (const rule of [...extraRules(opts.extraPatterns), ...BUILTIN]) {
    out = out.replace(rule.re, (...args) => {
      const m = args[0] as string;
      const groups = args.slice(1, -2) as string[];
      const r = rule.replace(m, ...groups);
      if (r !== m) bump(rule.name);
      return r;
    });
  }
  if (opts.emails !== false) {
    const allow = (opts.allowEmailDomains ?? envList("REDACT_ALLOW_EMAIL_DOMAINS")).map((d) => d.toLowerCase());
    out = out.replace(EMAIL, (m, domain: string) => {
      const d = domain.toLowerCase();
      if (allow.some((a) => d === a || d.endsWith("." + a))) return m;
      bump("EMAIL");
      return "[REDACTED-EMAIL]";
    });
  }
  return { text: out, counts };
}

export function redact(text: string, opts: RedactOptions = {}): string {
  return redactWithCounts(text, opts).text;
}

/** Redact every string inside a JSON-like value. */
export function redactDeep<T>(value: T, opts: RedactOptions = {}, counts?: RedactionCounts): T {
  if (typeof value === "string") {
    const r = redactWithCounts(value, opts);
    if (counts) for (const [k, v] of Object.entries(r.counts)) counts[k] = (counts[k] ?? 0) + v;
    return r.text as T;
  }
  if (Array.isArray(value)) return value.map((v) => redactDeep(v, opts, counts)) as T;
  if (value && typeof value === "object" && !(value instanceof Date)) {
    const out: Record<string, unknown> = {};
    for (const [k, v] of Object.entries(value as Record<string, unknown>)) out[k] = redactDeep(v, opts, counts);
    return out as T;
  }
  return value;
}

export function describeCounts(c: RedactionCounts): string {
  return Object.entries(c)
    .map(([k, v]) => `${k}×${v}`)
    .join(" ");
}
