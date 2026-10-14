import { env } from "./env";
import { basicAuthHeader, fetchOrThrow } from "./util";

/** "folder/sub/job" -> "<JENKINS_URL>/job/folder/job/sub/job/job". Full URLs are returned as-is. */
export function jobUrl(jobPath: string, base = env.jenkinsUrl): string {
  const p = jobPath.trim();
  if (/^https?:\/\//i.test(p)) return p.replace(/\/$/, "");
  if (!base) throw new Error("JENKINS_URL is not set");
  const segs = p
    .replace(/^\/+|\/+$/g, "")
    .split("/")
    .filter((s) => s && s !== "job")
    .map((s) => `job/${encodeURIComponent(s)}`);
  return `${base.replace(/\/$/, "")}/${segs.join("/")}`;
}

function auth(): HeadersInit {
  if (!env.jenkinsUser || !env.jenkinsToken) throw new Error("JENKINS_USER / JENKINS_API_TOKEN not set");
  return { Authorization: basicAuthHeader(env.jenkinsUser, env.jenkinsToken) };
}

async function crumb(): Promise<Record<string, string>> {
  // API tokens are exempt from CSRF on modern Jenkins, but older installs still want a crumb.
  try {
    const res = await fetchOrThrow(`${env.jenkinsUrl}/crumbIssuer/api/json`, { headers: auth(), timeoutMs: 10_000 });
    const j = (await res.json()) as { crumbRequestField: string; crumb: string };
    return { [j.crumbRequestField]: j.crumb };
  } catch {
    return {};
  }
}

/** Triggers a build and returns the queue item URL. */
export async function triggerBuild(jobPath: string, params: Record<string, string>): Promise<string> {
  const url = jobUrl(jobPath);
  const hasParams = Object.keys(params).length > 0;
  const body = new URLSearchParams(params);
  const res = await fetchOrThrow(`${url}/${hasParams ? "buildWithParameters" : "build"}`, {
    method: "POST",
    headers: { ...auth(), ...(await crumb()), "Content-Type": "application/x-www-form-urlencoded" },
    body: hasParams ? body.toString() : undefined,
    redirect: "manual",
  });
  const loc = res.headers.get("location");
  if (!loc) throw new Error(`Jenkins accepted the build but returned no queue location (status ${res.status})`);
  return loc.endsWith("/") ? loc : loc + "/";
}

export interface QueueState {
  cancelled: boolean;
  buildNumber?: number;
  buildUrl?: string;
  why?: string;
}

export async function queueItem(queueUrl: string): Promise<QueueState> {
  const res = await fetchOrThrow(`${queueUrl}api/json`, { headers: auth() });
  const j = (await res.json()) as {
    cancelled?: boolean;
    why?: string;
    executable?: { number: number; url: string } | null;
  };
  return {
    cancelled: Boolean(j.cancelled),
    why: j.why,
    buildNumber: j.executable?.number,
    buildUrl: j.executable?.url,
  };
}

export interface BuildState {
  building: boolean;
  result: string | null; // SUCCESS | FAILURE | UNSTABLE | ABORTED | NOT_BUILT | null
  durationMs: number;
}

export async function build(buildUrl: string): Promise<BuildState> {
  const base = buildUrl.endsWith("/") ? buildUrl : buildUrl + "/";
  const res = await fetchOrThrow(`${base}api/json?tree=building,result,duration`, { headers: auth() });
  const j = (await res.json()) as { building: boolean; result: string | null; duration: number };
  return { building: j.building, result: j.result, durationMs: j.duration };
}

export async function testReport(
  buildUrl: string,
): Promise<{ total: number; failed: number; skipped: number } | undefined> {
  const base = buildUrl.endsWith("/") ? buildUrl : buildUrl + "/";
  try {
    const res = await fetchOrThrow(`${base}testReport/api/json?tree=failCount,passCount,skipCount,totalCount`, {
      headers: auth(),
    });
    const j = (await res.json()) as { failCount: number; passCount: number; skipCount: number; totalCount?: number };
    return {
      total: j.totalCount ?? j.failCount + j.passCount + j.skipCount,
      failed: j.failCount,
      skipped: j.skipCount,
    };
  } catch {
    return undefined; // no test report published
  }
}

/** Last `lines` lines of a build's console log. */
export async function consoleTail(buildUrl: string, lines = 80): Promise<string> {
  const base = buildUrl.endsWith("/") ? buildUrl : buildUrl + "/";
  const res = await fetchOrThrow(`${base}consoleText`, { headers: auth(), timeoutMs: 30_000 });
  const text = await res.text();
  return text.split("\n").slice(-lines).join("\n");
}
