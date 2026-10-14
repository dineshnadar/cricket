/**
 * Centralised, lazily-read configuration. Values are read on access so that
 * `next build` works without a populated environment.
 */
function str(name: string, fallback = ""): string {
  const v = process.env[name];
  return v === undefined || v === "" ? fallback : v;
}
function num(name: string, fallback: number): number {
  const v = Number(process.env[name]);
  return Number.isFinite(v) && v > 0 ? v : fallback;
}
function bool(name: string, fallback = false): boolean {
  const v = process.env[name];
  if (!v) return fallback;
  return ["1", "true", "yes", "on"].includes(v.toLowerCase());
}

export const env = {
  get appBaseUrl() {
    return str("APP_BASE_URL", "http://localhost:3000").replace(/\/$/, "");
  },
  get appBasicAuth() {
    return str("APP_BASIC_AUTH");
  },
  get mongoUri() {
    return str("MONGODB_URI", "mongodb://localhost:27017");
  },
  get mongoDb() {
    return str("MONGODB_DB", "wisdom_ops");
  },
  get anthropicApiKey() {
    return str("ANTHROPIC_API_KEY");
  },
  get anthropicBaseUrl() {
    return str("ANTHROPIC_BASE_URL");
  },
  get anthropicModel() {
    return str("ANTHROPIC_MODEL", "claude-sonnet-5-5");
  },
  get githubApiUrl() {
    return str("GITHUB_API_URL", "https://api.github.com").replace(/\/$/, "");
  },
  get githubToken() {
    return str("GITHUB_TOKEN");
  },
  get githubOrgs() {
    return str("GITHUB_ORGS")
      .split(",")
      .map((s) => s.trim())
      .filter(Boolean);
  },
  get jenkinsUrl() {
    return str("JENKINS_URL").replace(/\/$/, "");
  },
  get jenkinsUser() {
    return str("JENKINS_USER");
  },
  get jenkinsToken() {
    return str("JENKINS_API_TOKEN");
  },
  get teamcityUrl() {
    return str("TEAMCITY_URL").replace(/\/$/, "");
  },
  get teamcityToken() {
    return str("TEAMCITY_TOKEN");
  },
  get smtpHost() {
    return str("SMTP_HOST");
  },
  get smtpPort() {
    return num("SMTP_PORT", 25);
  },
  get smtpSecure() {
    return bool("SMTP_SECURE");
  },
  get smtpUser() {
    return str("SMTP_USER");
  },
  get smtpPass() {
    return str("SMTP_PASS");
  },
  get smtpFrom() {
    return str("SMTP_FROM", "Wisdom Ops <wisdom-ops@localhost>");
  },
  get servicenowUrl() {
    return str("SERVICENOW_URL").replace(/\/$/, "");
  },
  get servicenowUser() {
    return str("SERVICENOW_USER");
  },
  get servicenowPassword() {
    return str("SERVICENOW_PASSWORD");
  },
  get splunkUrl() {
    return str("SPLUNK_URL").replace(/\/$/, "");
  },
  get splunkToken() {
    return str("SPLUNK_TOKEN");
  },
  get splunkUser() {
    return str("SPLUNK_USER");
  },
  get splunkPassword() {
    return str("SPLUNK_PASSWORD");
  },
  get workerTickSeconds() {
    return num("WORKER_TICK_SECONDS", 15);
  },
  get pipelineRefreshSeconds() {
    return num("PIPELINE_REFRESH_SECONDS", 30);
  },
  get servicenowPollMinutes() {
    return num("SERVICENOW_POLL_MINUTES", 5);
  },
  get servicenowLookbackMinutes() {
    return num("SERVICENOW_LOOKBACK_MINUTES", 120);
  },
  get wisdomRefreshHours() {
    return num("WISDOM_REFRESH_HOURS", 24);
  },
  get dryRun() {
    return bool("DRY_RUN");
  },
};

export function integrationStatus() {
  return {
    claude: Boolean(env.anthropicApiKey),
    github: Boolean(env.githubToken),
    jenkins: Boolean(env.jenkinsUrl && env.jenkinsUser && env.jenkinsToken),
    teamcity: Boolean(env.teamcityUrl && env.teamcityToken),
    smtp: Boolean(env.smtpHost),
    servicenow: Boolean(env.servicenowUrl && env.servicenowUser),
    splunk: Boolean(env.splunkUrl && (env.splunkToken || env.splunkUser)),
    dryRun: env.dryRun,
  };
}
