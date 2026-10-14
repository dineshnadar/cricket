import picomatch from "picomatch";
import type { Agent, WatchedRepo } from "./types";

export function findRepoConfig(agent: Pick<Agent, "repos">, fullName: string): WatchedRepo | undefined {
  const target = fullName.toLowerCase();
  return agent.repos.find((r) => r.fullName.toLowerCase() === target);
}

/** Branch is watched if no branches configured (any) or it matches one (globs allowed, e.g. "release/*"). */
export function branchWatched(repo: WatchedRepo, branch: string, defaultBranch?: string): boolean {
  if (!repo.branches.length) return defaultBranch ? branch === defaultBranch : true;
  return repo.branches.some((b) => picomatch.isMatch(branch, b));
}

/** Returns the files that match the repo's path globs (all files if no globs). */
export function matchWatchedFiles(repo: WatchedRepo, files: string[]): string[] {
  const globs = repo.paths.map((p) => p.trim()).filter(Boolean);
  if (!globs.length) return files;
  const include = globs.filter((g) => !g.startsWith("!"));
  const exclude = globs.filter((g) => g.startsWith("!")).map((g) => g.slice(1));
  const inc = include.length ? picomatch(include, { dot: true }) : () => true;
  const exc = exclude.length ? picomatch(exclude, { dot: true }) : () => false;
  return files.filter((f) => inc(f) && !exc(f));
}
