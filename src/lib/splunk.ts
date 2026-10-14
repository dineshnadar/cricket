import { env } from "./env";
import type { SplunkEvent } from "./types";
import { basicAuthHeader, fetchOrThrow, truncate } from "./util";

/** Quote a keyword for SPL. */
export function splQuote(term: string): string {
  return `"${term.replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
}

export function buildSearch(baseSearch: string, terms: string[], maxEvents: number): string {
  const base = baseSearch.trim();
  const prefix = base.startsWith("|") || /^search\s/i.test(base) ? base : `search ${base}`;
  const kw = terms.filter(Boolean).slice(0, 6).map(splQuote);
  const kwClause = kw.length ? ` (${kw.join(" OR ")})` : "";
  return `${prefix}${kwClause} | head ${maxEvents}`;
}

function authHeader(): string {
  if (env.splunkToken) return `Bearer ${env.splunkToken}`;
  if (env.splunkUser) return basicAuthHeader(env.splunkUser, env.splunkPassword);
  throw new Error("SPLUNK_TOKEN or SPLUNK_USER not set");
}

/** Parse the newline-delimited JSON stream from /search/jobs/export. */
export function parseExport(text: string, maxRaw = 2000): SplunkEvent[] {
  const out: SplunkEvent[] = [];
  for (const line of text.split("\n")) {
    const l = line.trim();
    if (!l) continue;
    try {
      const j = JSON.parse(l) as { result?: Record<string, string> };
      if (j.result) {
        out.push({
          time: j.result._time,
          host: j.result.host,
          source: j.result.source,
          raw: truncate(j.result._raw ?? JSON.stringify(j.result), maxRaw),
        });
      }
    } catch {
      /* skip partial lines */
    }
  }
  return out;
}

export async function search(spl: string, earliest: Date, latest: Date): Promise<SplunkEvent[]> {
  if (!env.splunkUrl) throw new Error("SPLUNK_URL not set");
  const body = new URLSearchParams({
    search: spl,
    earliest_time: earliest.toISOString(),
    latest_time: latest.toISOString(),
    output_mode: "json",
  });
  const res = await fetchOrThrow(`${env.splunkUrl}/services/search/jobs/export`, {
    method: "POST",
    headers: { Authorization: authHeader(), "Content-Type": "application/x-www-form-urlencoded" },
    body: body.toString(),
    timeoutMs: 120_000,
  });
  return parseExport(await res.text());
}
