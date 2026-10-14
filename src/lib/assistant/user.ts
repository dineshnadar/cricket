/** No SSO yet: the browser sends a self-declared display name, used for audit and to scope conversation history. */
export function requestUser(req: Request): string {
  const raw = req.headers.get("x-wisdom-user") ?? "";
  const clean = raw.replace(/[^\p{L}\p{N} ._@-]/gu, "").trim().slice(0, 60);
  return clean || "anonymous";
}
