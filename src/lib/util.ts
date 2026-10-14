import { createHash, randomBytes, timingSafeEqual } from "node:crypto";

export function newToken(): string {
  return "wo_" + randomBytes(24).toString("base64url");
}

export function hashToken(token: string): string {
  return createHash("sha256").update(token).digest("hex");
}

export function tokenMatches(token: string, hash: string): boolean {
  const a = Buffer.from(hashToken(token), "hex");
  const b = Buffer.from(hash, "hex");
  return a.length === b.length && timingSafeEqual(a, b);
}

export function truncate(s: string | undefined | null, max: number): string {
  if (!s) return "";
  return s.length <= max ? s : s.slice(0, max) + `\n…[truncated ${s.length - max} chars]`;
}

export function shortSha(sha?: string): string {
  return sha ? sha.slice(0, 7) : "";
}

export function escapeHtml(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

export function basicAuthHeader(user: string, pass: string): string {
  return "Basic " + Buffer.from(`${user}:${pass}`).toString("base64");
}

export function errMsg(e: unknown): string {
  if (e instanceof Error) return e.message;
  return String(e);
}

export async function fetchOrThrow(url: string, init: RequestInit & { timeoutMs?: number } = {}): Promise<Response> {
  const { timeoutMs = 30_000, ...rest } = init;
  const res = await fetch(url, { ...rest, signal: AbortSignal.timeout(timeoutMs) });
  if (!res.ok) {
    const body = await res.text().catch(() => "");
    throw new Error(`${rest.method ?? "GET"} ${url} -> ${res.status} ${res.statusText} ${truncate(body, 500)}`);
  }
  return res;
}

export function fmtDate(d?: Date | string | null): string {
  if (!d) return "—";
  const date = typeof d === "string" ? new Date(d) : d;
  return date.toLocaleString("en-US", {
    month: "short",
    day: "numeric",
    hour: "numeric",
    minute: "2-digit",
  });
}

export function ago(d?: Date | string | null): string {
  if (!d) return "—";
  const date = typeof d === "string" ? new Date(d) : d;
  const s = Math.round((Date.now() - date.getTime()) / 1000);
  if (s < 60) return `${s}s ago`;
  if (s < 3600) return `${Math.round(s / 60)}m ago`;
  if (s < 86400) return `${Math.round(s / 3600)}h ago`;
  return `${Math.round(s / 86400)}d ago`;
}

export const ZERO_SHA = "0000000000000000000000000000000000000000";

export function plural(n: number, word: string, pluralWord = word + "s"): string {
  return `${n} ${n === 1 ? word : pluralWord}`;
}
