import { existsSync, readFileSync } from "node:fs";

/**
 * Next.js loads .env files itself; the worker and scripts (run with tsx) don't.
 * Load .env.local then .env, never overriding variables already set.
 */
export function loadEnvFiles(files = [".env.local", ".env"]) {
  for (const f of files) {
    if (!existsSync(f)) continue;
    for (const raw of readFileSync(f, "utf8").split(/\r?\n/)) {
      const line = raw.trim();
      if (!line || line.startsWith("#")) continue;
      const eq = line.indexOf("=");
      if (eq < 1) continue;
      const key = line.slice(0, eq).trim();
      let val = line.slice(eq + 1).trim();
      if ((val.startsWith('"') && val.endsWith('"')) || (val.startsWith("'") && val.endsWith("'"))) val = val.slice(1, -1);
      if (process.env[key] === undefined) process.env[key] = val;
    }
  }
}
