import nodemailer from "nodemailer";
import { env } from "./env";
import type { Agent, Change, Incident, NotificationRecord, PipelineRun } from "./types";
import { errMsg, escapeHtml, fetchOrThrow, shortSha } from "./util";

export interface Message {
  subject: string;
  /** Short markdown-ish body (Teams) */
  title: string;
  facts: [string, string][];
  /** Markdown text (rendered as-is in Teams, converted lightly for email) */
  body: string;
  link: { label: string; url: string };
  severity: "info" | "good" | "warning" | "attention";
}

let transporter: nodemailer.Transporter | null = null;
function mailer() {
  if (!env.smtpHost) throw new Error("SMTP_HOST is not set");
  if (!transporter) {
    transporter = nodemailer.createTransport({
      host: env.smtpHost,
      port: env.smtpPort,
      secure: env.smtpSecure,
      auth: env.smtpUser ? { user: env.smtpUser, pass: env.smtpPass } : undefined,
    });
  }
  return transporter;
}

/** Minimal markdown -> HTML for bullets, bold and inline code. */
export function mdToHtml(md: string): string {
  const lines = escapeHtml(md).split("\n");
  const out: string[] = [];
  let inList = false;
  for (const raw of lines) {
    const line = raw
      .replace(/\*\*(.+?)\*\*/g, "<strong>$1</strong>")
      .replace(/`([^`]+)`/g, "<code>$1</code>");
    const bullet = line.match(/^\s*[-*]\s+(.*)$/);
    if (bullet) {
      if (!inList) out.push("<ul>");
      inList = true;
      out.push(`<li>${bullet[1]}</li>`);
    } else {
      if (inList) out.push("</ul>");
      inList = false;
      if (line.trim()) out.push(`<p>${line}</p>`);
    }
  }
  if (inList) out.push("</ul>");
  return out.join("\n");
}

const sevColor: Record<Message["severity"], string> = {
  info: "#2563eb",
  good: "#15803d",
  warning: "#b45309",
  attention: "#b91c1c",
};

export function emailHtml(m: Message): string {
  const facts = m.facts
    .map(
      ([k, v]) =>
        `<tr><td style="padding:2px 12px 2px 0;color:#555;white-space:nowrap">${escapeHtml(k)}</td><td>${escapeHtml(v)}</td></tr>`,
    )
    .join("");
  return `<div style="font-family:Segoe UI,Arial,sans-serif;font-size:14px;color:#111;max-width:720px">
<div style="border-left:4px solid ${sevColor[m.severity]};padding-left:12px">
<h2 style="margin:0 0 8px;font-size:18px">${escapeHtml(m.title)}</h2>
<table style="font-size:13px;margin-bottom:12px">${facts}</table>
</div>
${mdToHtml(m.body)}
<p><a href="${escapeHtml(m.link.url)}">${escapeHtml(m.link.label)}</a></p>
<p style="color:#888;font-size:12px">Sent by Wisdom Ops</p>
</div>`;
}

export function teamsCard(m: Message) {
  const color = { info: "Accent", good: "Good", warning: "Warning", attention: "Attention" }[m.severity];
  return {
    type: "message",
    attachments: [
      {
        contentType: "application/vnd.microsoft.card.adaptive",
        content: {
          $schema: "http://adaptivecards.io/schemas/adaptive-card.json",
          type: "AdaptiveCard",
          version: "1.4",
          msteams: { width: "Full" },
          body: [
            { type: "TextBlock", text: m.title, weight: "Bolder", size: "Medium", color, wrap: true },
            { type: "FactSet", facts: m.facts.map(([title, value]) => ({ title, value })) },
            { type: "TextBlock", text: m.body, wrap: true },
          ],
          actions: [{ type: "Action.OpenUrl", title: m.link.label, url: m.link.url }],
        },
      },
    ],
  };
}

export async function sendEmail(to: string[], m: Message): Promise<NotificationRecord | null> {
  if (!to.length) return null;
  const at = new Date();
  if (env.dryRun) {
    console.log(`[dry-run] email to ${to.join(", ")}: ${m.subject}`);
    return { channel: "email", ok: true, detail: "dry-run", at };
  }
  try {
    const info = await mailer().sendMail({
      from: env.smtpFrom,
      to: to.join(", "),
      subject: m.subject,
      html: emailHtml(m),
      text: `${m.title}\n\n${m.facts.map(([k, v]) => `${k}: ${v}`).join("\n")}\n\n${m.body}\n\n${m.link.url}`,
    });
    return { channel: "email", ok: true, detail: info.messageId, at };
  } catch (e) {
    return { channel: "email", ok: false, detail: errMsg(e), at };
  }
}

export async function sendTeams(webhookUrl: string, m: Message): Promise<NotificationRecord | null> {
  if (!webhookUrl) return null;
  const at = new Date();
  if (env.dryRun) {
    console.log(`[dry-run] teams: ${m.title}`);
    return { channel: "teams", ok: true, detail: "dry-run", at };
  }
  try {
    await fetchOrThrow(webhookUrl, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(teamsCard(m)),
    });
    return { channel: "teams", ok: true, at };
  } catch (e) {
    return { channel: "teams", ok: false, detail: errMsg(e), at };
  }
}

export async function notifyAgent(agent: Agent, m: Message): Promise<NotificationRecord[]> {
  const results = await Promise.all([sendEmail(agent.emailRecipients, m), sendTeams(agent.teamsWebhookUrl, m)]);
  return results.filter((r): r is NotificationRecord => r !== null);
}

// ---------- message builders ----------

/** Commit authors of a change, falling back to the pusher. */
export function changeAuthors(change: Change): string {
  return [...new Set(change.commits.map((c) => c.author).filter(Boolean))].join(", ") || change.pusher || "unknown";
}

export function changeMessage(agent: Agent, change: Change, runs: PipelineRun[]): Message {
  const t = change.triage!;
  const sev: Message["severity"] = t.riskLevel === "high" ? "attention" : t.riskLevel === "medium" ? "warning" : "info";
  const authors = changeAuthors(change);
  const runsText = runs.length
    ? "\n\n**Pipelines triggered:**\n" + runs.map((r) => `- ${r.pipelineName}: ${r.status}`).join("\n")
    : "";
  const focus = t.testFocus.length ? "\n\n**Test focus:**\n" + t.testFocus.map((f) => `- ${f}`).join("\n") : "";
  return {
    subject: `[${agent.module}] ${t.riskLevel.toUpperCase()} risk change in ${change.repo}@${change.branch} (${shortSha(change.after)})`,
    title: `${agent.module}: ${t.riskLevel} risk change in ${change.repo}`,
    facts: [
      ["Repo", `${change.repo} @ ${change.branch}`],
      ["Commits", `${change.commits.length} by ${authors}`],
      ["Files", `${change.matchedFiles.length} watched / ${change.files.length} total`],
      ["Areas", t.impactedAreas.join(", ") || "—"],
      ["Confidence", `${Math.round(t.confidence * 100)}%`],
    ],
    body: t.summary + focus + runsText,
    link: { label: "Open in Wisdom Ops", url: `${env.appBaseUrl}/changes/${change._id}` },
    severity: sev,
  };
}

export function runMessage(agent: Agent, run: PipelineRun, change: Change | null): Message {
  const ok = run.status === "success";
  const tests = run.tests ? `${run.tests.failed} failed / ${run.tests.total} tests` : "no test report";
  return {
    subject: `[${agent.module}] ${run.pipelineName} ${run.status.toUpperCase()}${change ? ` for ${change.repo}@${shortSha(change.after)}` : ""}`,
    title: `${run.pipelineName}: ${run.status}`,
    facts: [
      ["Pipeline", run.pipelineName],
      ["Result", run.status],
      ["Tests", tests],
      ...(change
        ? ([
            ["Change", `${change.repo} @ ${change.branch} (${shortSha(change.after)})`],
            ["Author", changeAuthors(change)],
          ] as [string, string][])
        : []),
    ],
    body: change?.triage?.summary ?? "",
    link: run.buildUrl
      ? { label: "Open build", url: run.buildUrl }
      : { label: "Open in Wisdom Ops", url: `${env.appBaseUrl}/changes/${run.changeId}` },
    severity: ok ? "good" : "attention",
  };
}

export function incidentMessage(agent: Agent, inc: Incident): Message {
  const a = inc.analysis!;
  const top = a.likelyCauses[0];
  const causes = a.likelyCauses.length
    ? "\n\n**Likely causes:**\n" +
      a.likelyCauses
        .map((c) => `- ${c.repo ? `${c.repo}@${shortSha(c.sha)}: ` : ""}${c.reason} (${Math.round(c.confidence * 100)}%)`)
        .join("\n")
    : "";
  const actions = a.suggestedActions.length
    ? "\n\n**Suggested next steps:**\n" + a.suggestedActions.map((s) => `- ${s}`).join("\n")
    : "";
  return {
    subject: `[${agent.module}] ${inc.number}: ${inc.shortDescription}`,
    title: `${inc.number}: ${inc.shortDescription}`,
    facts: [
      ["Priority", inc.priority ?? "—"],
      ["Opened", inc.openedAt.toISOString()],
      ["Top suspect", top ? `${top.repo ?? "?"} (${Math.round(top.confidence * 100)}%)` : "none found"],
      ["Splunk events", String(inc.splunkEvents.length)],
    ],
    body: a.summary + causes + actions,
    link: { label: "Open analysis", url: `${env.appBaseUrl}/incidents/${inc._id}` },
    severity: "attention",
  };
}
