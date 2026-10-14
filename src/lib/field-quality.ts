/**
 * Cheap "how good is this?" check for the two free-text fields Claude relies on
 * during triage. No AI: runs in the browser on every keystroke.
 */

export type FieldKind = "module" | "repoNotes";
export type QualityLevel = "empty" | "vague" | "ok" | "good";

export interface Quality {
  level: QualityLevel;
  label: string;
  tip: string;
}

const words = (s: string) => s.trim().split(/\s+/).filter(Boolean).length;
const has = (s: string, re: RegExp) => re.test(s);

/** Code-ish names: PoaValidator, /api/poa, card_limits, isExpired(), *.yml */
const CODE_REF = /([A-Z][a-z]+[A-Z]\w+|\/[\w-]+\/|\w+_\w+|\w+\(\)|\.\w{2,4}\b)/;
const OUT_OF_SCOPE = /\b(out of scope|not in scope|excludes?|excluding|not included|ignore|except)\b/i;
const OWNER = /\b(owner|owned by|team|contact|maintained by)\b/i;

export function fieldQuality(kind: FieldKind, text: string): Quality {
  const n = words(text);
  if (!n) {
    return {
      level: "empty",
      label: "Empty",
      tip: kind === "module" ? "Wisdom AI will have to guess what's relevant. Write a rough line and use Improve with AI." : "Use Draft from repo: it reads the README and folders for you.",
    };
  }
  const ref = has(text, CODE_REF);
  if (kind === "module") {
    const scope = has(text, OUT_OF_SCOPE);
    if (n < 12) return { level: "vague", label: "Too vague", tip: "Triage will guess. Name the sub-areas and what's out of scope, or use Improve with AI." };
    if (!scope) return { level: "ok", label: "OK", tip: "Add what's out of scope so Wisdom AI can ignore unrelated changes." };
    return n >= 30 ? { level: "good", label: "Good", tip: "Specific enough for reliable triage." } : { level: "ok", label: "OK", tip: "A little more detail on critical flows would help." };
  }
  const owner = has(text, OWNER);
  if (n < 8) return { level: "vague", label: "Too vague", tip: "Name the classes, endpoints or config that matter, or use Draft from repo." };
  if (!ref) return { level: "ok", label: "OK", tip: "Mention specific classes, endpoints or files so Wisdom AI can spot risky changes." };
  if (!owner) return { level: "ok", label: "OK", tip: "Add the owning team so alerts can name them." };
  return { level: "good", label: "Good", tip: "Specific enough for reliable triage." };
}
