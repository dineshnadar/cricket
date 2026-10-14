import { env } from "./env";
import { basicAuthHeader, fetchOrThrow } from "./util";

export interface SnIncident {
  sys_id: string;
  number: string;
  short_description: string;
  description: string;
  priority: string;
  state: string;
  opened_at: string; // "YYYY-MM-DD HH:mm:ss" in UTC when sysparm_display_value=false
}

function headers(): HeadersInit {
  if (!env.servicenowUrl || !env.servicenowUser) throw new Error("SERVICENOW_URL / SERVICENOW_USER not set");
  return {
    Accept: "application/json",
    "Content-Type": "application/json",
    Authorization: basicAuthHeader(env.servicenowUser, env.servicenowPassword),
  };
}

/** Incidents matching the agent's encoded query created in the last `minutes`. */
export async function recentIncidents(query: string, minutes: number): Promise<SnIncident[]> {
  const q = [query.trim(), `sys_created_on>=javascript:gs.minutesAgoStart(${Math.ceil(minutes)})`]
    .filter(Boolean)
    .join("^");
  const params = new URLSearchParams({
    sysparm_query: q + "^ORDERBYDESCsys_created_on",
    sysparm_fields: "sys_id,number,short_description,description,priority,state,opened_at",
    sysparm_limit: "100",
    sysparm_display_value: "false",
  });
  const res = await fetchOrThrow(`${env.servicenowUrl}/api/now/table/incident?${params}`, { headers: headers() });
  const j = (await res.json()) as { result: SnIncident[] };
  return j.result;
}

export function parseSnDate(s: string): Date {
  // ServiceNow returns UTC "YYYY-MM-DD HH:mm:ss" with display_value=false
  const d = new Date(s.replace(" ", "T") + "Z");
  return Number.isNaN(d.getTime()) ? new Date() : d;
}

export function incidentUrl(sysId: string): string {
  return `${env.servicenowUrl}/nav_to.do?uri=incident.do?sys_id=${sysId}`;
}

export async function addWorkNote(sysId: string, note: string): Promise<void> {
  await fetchOrThrow(`${env.servicenowUrl}/api/now/table/incident/${sysId}`, {
    method: "PATCH",
    headers: headers(),
    body: JSON.stringify({ work_notes: note }),
  });
}
