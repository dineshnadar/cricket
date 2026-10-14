import { env } from "./env";
import type { ChangeCommit, ChangeFile } from "./types";
import { fetchOrThrow } from "./util";

function headers(accept = "application/vnd.github+json"): HeadersInit {
  if (!env.githubToken) throw new Error("GITHUB_TOKEN is not set");
  return {
    Accept: accept,
    Authorization: `Bearer ${env.githubToken}`,
    "X-GitHub-Api-Version": "2022-11-28",
  };
}

async function gh<T>(path: string, init: RequestInit = {}): Promise<T> {
  const res = await fetchOrThrow(`${env.githubApiUrl}${path}`, {
    ...init,
    headers: { ...headers(), ...(init.headers ?? {}) },
  });
  if (res.status === 204) return undefined as T;
  return (await res.json()) as T;
}

export interface RepoSummary {
  fullName: string;
  description: string | null;
  defaultBranch: string;
  private: boolean;
  pushedAt: string | null;
  url: string;
}

interface GhRepo {
  full_name: string;
  description: string | null;
  default_branch: string;
  private: boolean;
  pushed_at: string | null;
  html_url: string;
}

const mapRepo = (r: GhRepo): RepoSummary => ({
  fullName: r.full_name,
  description: r.description,
  defaultBranch: r.default_branch,
  private: r.private,
  pushedAt: r.pushed_at,
  url: r.html_url,
});

/** Repo picker: searches within configured orgs (or everything the token can see). */
export async function searchRepos(query: string): Promise<RepoSummary[]> {
  const q = query.trim();
  const orgs = env.githubOrgs;
  if (!q && orgs.length) {
    const lists = await Promise.all(
      orgs.map((o) => gh<GhRepo[]>(`/orgs/${encodeURIComponent(o)}/repos?per_page=100&sort=pushed`).catch(() => [])),
    );
    return lists.flat().map(mapRepo);
  }
  if (!q) {
    const repos = await gh<GhRepo[]>(`/user/repos?per_page=100&sort=pushed`);
    return repos.map(mapRepo);
  }
  const scope = orgs.map((o) => `org:${o}`).join(" ");
  const res = await gh<{ items: GhRepo[] }>(
    `/search/repositories?per_page=50&q=${encodeURIComponent(`${q} in:name ${scope}`.trim())}`,
  );
  return res.items.map(mapRepo);
}

export async function getRepo(fullName: string): Promise<RepoSummary> {
  return mapRepo(await gh<GhRepo>(`/repos/${fullName}`));
}

/**
 * Check that each repo exists and the token can see it. Returns the ones that
 * don't (404/403). Throws if GitHub can't be reached at all, so a setup is never
 * confirmed without a real check.
 */
export async function findMissingRepos(fullNames: string[]): Promise<string[]> {
  const missing: string[] = [];
  for (const name of [...new Set(fullNames)]) {
    try {
      await getRepo(name);
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      if (/ -> (404|403|451) /.test(msg)) missing.push(name);
      else throw new Error(`Couldn't check ${name} on GitHub: ${msg}`);
    }
  }
  return missing;
}

export async function listBranches(fullName: string): Promise<string[]> {
  const res = await gh<{ name: string }[]>(`/repos/${fullName}/branches?per_page=100`);
  return res.map((b) => b.name);
}

export async function headSha(fullName: string, branch: string): Promise<string> {
  const res = await gh<{ sha: string }>(`/repos/${fullName}/commits/${encodeURIComponent(branch)}`);
  return res.sha;
}

interface GhCommit {
  sha: string;
  html_url: string;
  commit: { message: string; author: { name: string; email: string; date: string } | null };
  author: { login: string } | null;
}
interface GhFile {
  filename: string;
  status: string;
  additions: number;
  deletions: number;
  patch?: string;
}

const mapCommit = (c: GhCommit): ChangeCommit => ({
  sha: c.sha,
  message: c.commit.message,
  author: c.author?.login ?? c.commit.author?.name ?? "unknown",
  email: c.commit.author?.email,
  timestamp: c.commit.author?.date,
  url: c.html_url,
});

export async function compare(
  fullName: string,
  base: string,
  head: string,
): Promise<{ commits: ChangeCommit[]; files: ChangeFile[]; url: string }> {
  const res = await gh<{ commits: GhCommit[]; files?: GhFile[]; html_url: string }>(
    `/repos/${fullName}/compare/${encodeURIComponent(base)}...${encodeURIComponent(head)}`,
  );
  return {
    commits: res.commits.map(mapCommit),
    files: (res.files ?? []).map((f) => ({
      filename: f.filename,
      status: f.status,
      additions: f.additions,
      deletions: f.deletions,
      patch: f.patch,
    })),
    url: res.html_url,
  };
}

export async function recentCommits(fullName: string, branch: string, count = 30): Promise<ChangeCommit[]> {
  const res = await gh<GhCommit[]>(
    `/repos/${fullName}/commits?sha=${encodeURIComponent(branch)}&per_page=${Math.min(count, 100)}`,
  );
  return res.map(mapCommit);
}

export async function readme(fullName: string): Promise<string> {
  try {
    const res = await fetchOrThrow(`${env.githubApiUrl}/repos/${fullName}/readme`, {
      headers: headers("application/vnd.github.raw+json"),
    });
    return await res.text();
  } catch {
    return "";
  }
}

export async function tree(fullName: string, branch: string): Promise<string[]> {
  const res = await gh<{ tree: { path: string; type: string }[] }>(
    `/repos/${fullName}/git/trees/${encodeURIComponent(branch)}?recursive=1`,
  );
  return res.tree.filter((t) => t.type === "blob").map((t) => t.path);
}

export async function dispatchWorkflow(
  fullName: string,
  workflow: string,
  ref: string,
  inputs: Record<string, string>,
): Promise<void> {
  await gh<void>(`/repos/${fullName}/actions/workflows/${encodeURIComponent(workflow)}/dispatches`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ ref, inputs }),
  });
}

export interface WorkflowRun {
  id: number;
  status: string; // queued | in_progress | completed | ...
  conclusion: string | null; // success | failure | cancelled | ...
  html_url: string;
  created_at: string;
  run_number: number;
}

export async function findDispatchedRun(
  fullName: string,
  workflow: string,
  ref: string,
  since: Date,
): Promise<WorkflowRun | undefined> {
  const created = new Date(since.getTime() - 10_000).toISOString();
  const res = await gh<{ workflow_runs: WorkflowRun[] }>(
    `/repos/${fullName}/actions/workflows/${encodeURIComponent(workflow)}/runs?event=workflow_dispatch&branch=${encodeURIComponent(ref)}&created=${encodeURIComponent(">=" + created)}&per_page=10`,
  );
  return res.workflow_runs.sort((a, b) => a.created_at.localeCompare(b.created_at))[0];
}

export async function getWorkflowRun(fullName: string, runId: number): Promise<WorkflowRun> {
  return gh<WorkflowRun>(`/repos/${fullName}/actions/runs/${runId}`);
}

/** Raw file contents at a ref (default branch when ref is omitted). */
export async function readFile(fullName: string, path: string, ref?: string): Promise<string> {
  const q = ref ? `?ref=${encodeURIComponent(ref)}` : "";
  const res = await fetchOrThrow(
    `${env.githubApiUrl}/repos/${fullName}/contents/${path.split("/").map(encodeURIComponent).join("/")}${q}`,
    { headers: headers("application/vnd.github.raw+json") },
  );
  return res.text();
}

/**
 * Opens (or reuses) a PR that adds the Wisdom Ops workflow to a repo.
 * Needs a token with contents:write and pull_requests:write on the repo.
 */
export async function openWorkflowPr(
  fullName: string,
  workflowYaml: string,
  opts: { path?: string; branch?: string; title?: string; body?: string } = {},
): Promise<{ url: string; number: number; reused: boolean }> {
  const path = opts.path ?? ".github/workflows/wisdom-ops.yml";
  const branch = opts.branch ?? "wisdom-ops/setup";
  const repo = await getRepo(fullName);

  const existing = await gh<{ html_url: string; number: number }[]>(
    `/repos/${fullName}/pulls?state=open&head=${encodeURIComponent(fullName.split("/")[0] + ":" + branch)}`,
  );
  if (existing.length) return { url: existing[0].html_url, number: existing[0].number, reused: true };

  const base = await gh<{ object: { sha: string } }>(
    `/repos/${fullName}/git/ref/heads/${encodeURIComponent(repo.defaultBranch)}`,
  );
  try {
    await gh(`/repos/${fullName}/git/refs`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ ref: `refs/heads/${branch}`, sha: base.object.sha }),
    });
  } catch (e) {
    if (!String(e).includes("422")) throw e; // branch already exists: reuse it
  }

  let sha: string | undefined;
  try {
    const cur = await gh<{ sha: string }>(`/repos/${fullName}/contents/${path}?ref=${encodeURIComponent(branch)}`);
    sha = cur.sha;
  } catch {
    sha = undefined;
  }
  await gh(`/repos/${fullName}/contents/${path}`, {
    method: "PUT",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      message: "Add Wisdom Ops change-notify workflow",
      content: Buffer.from(workflowYaml).toString("base64"),
      branch,
      ...(sha ? { sha } : {}),
    }),
  });
  const pr = await gh<{ html_url: string; number: number }>(`/repos/${fullName}/pulls`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      title: opts.title ?? "Add Wisdom Ops change notifications",
      head: branch,
      base: repo.defaultBranch,
      body:
        opts.body ??
        "Adds a workflow that sends each push to Wisdom Ops for triage. Requires the `WISDOM_TOKEN` secret (repo or org level).",
    }),
  });
  return { url: pr.html_url, number: pr.number, reused: false };
}
