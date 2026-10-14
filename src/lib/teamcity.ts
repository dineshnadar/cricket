import { env } from "./env";
import { fetchOrThrow } from "./util";

/**
 * TeamCity REST client (token auth: Profile → Access Tokens, or a service account token).
 * Docs: <TEAMCITY_URL>/app/rest/swagger.json on your server.
 */

function headers(): HeadersInit {
  if (!env.teamcityUrl || !env.teamcityToken) throw new Error("TEAMCITY_URL / TEAMCITY_TOKEN not set");
  return {
    Authorization: `Bearer ${env.teamcityToken}`,
    Accept: "application/json",
    "Content-Type": "application/json",
  };
}

/**
 * Map our params to TeamCity build properties. Names are stored without dots (Mongo-safe),
 * so the prefix is applied here: WISDOM_* context always becomes env.WISDOM_* (environment
 * variables in build steps); the pipeline's own params get its prefix (default "env.").
 */
export function toProperties(params: Record<string, string>, prefix: "env." | "system." | "" = "env."): { name: string; value: string }[] {
  return Object.entries(params).map(([k, v]) => ({
    name: k.startsWith("WISDOM_") ? `env.${k}` : `${prefix}${k}`,
    value: String(v),
  }));
}

export interface TcBuild {
  id: number;
  number?: string;
  state: "queued" | "running" | "finished" | string;
  status?: "SUCCESS" | "FAILURE" | "UNKNOWN" | "ERROR" | string;
  statusText?: string;
  webUrl?: string;
  canceledInfo?: unknown;
  testOccurrences?: { count?: number; passed?: number; failed?: number; ignored?: number; muted?: number };
}

/** Queue a build. `branchName` is optional (omit for the build config's default branch). */
export async function triggerBuild(
  buildTypeId: string,
  params: Record<string, string>,
  branchName?: string,
  prefix: "env." | "system." | "" = "env.",
): Promise<TcBuild> {
  const body: Record<string, unknown> = {
    buildType: { id: buildTypeId },
    properties: { property: toProperties(params, prefix) },
    comment: { text: `Triggered by Wisdom Ops${params.WISDOM_REPO ? ` for ${params.WISDOM_REPO}@${(params.WISDOM_SHA ?? "").slice(0, 7)}` : ""}` },
  };
  if (branchName) body.branchName = branchName;
  const res = await fetchOrThrow(`${env.teamcityUrl}/app/rest/buildQueue`, {
    method: "POST",
    headers: headers(),
    body: JSON.stringify(body),
  });
  return (await res.json()) as TcBuild;
}

export async function getBuild(id: number): Promise<TcBuild> {
  const fields = "id,number,state,status,statusText,webUrl,canceledInfo,testOccurrences(count,passed,failed,ignored,muted)";
  const res = await fetchOrThrow(`${env.teamcityUrl}/app/rest/builds/id:${id}?fields=${encodeURIComponent(fields)}`, {
    headers: headers(),
  });
  return (await res.json()) as TcBuild;
}

export function testsOf(b: TcBuild): { total: number; failed: number; skipped: number } | undefined {
  const t = b.testOccurrences;
  if (!t || t.count === undefined) return undefined;
  return { total: t.count, failed: t.failed ?? 0, skipped: (t.ignored ?? 0) + (t.muted ?? 0) };
}

/** Last `lines` lines of a build log. */
export async function logTail(id: number, lines = 80): Promise<string> {
  const res = await fetchOrThrow(`${env.teamcityUrl}/downloadBuildLog.html?buildId=${id}`, {
    headers: { Authorization: `Bearer ${env.teamcityToken}`, Accept: "text/plain" },
    timeoutMs: 30_000,
  });
  return (await res.text()).split("\n").slice(-lines).join("\n");
}
