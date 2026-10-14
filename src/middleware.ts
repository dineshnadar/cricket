import { NextResponse, type NextRequest } from "next/server";

/**
 * Optional basic auth for the UI and management API (APP_BASIC_AUTH="user:pass").
 * The ingest endpoint authenticates with per-agent tokens, and /api/health is open.
 */
export function middleware(req: NextRequest) {
  const creds = process.env.APP_BASIC_AUTH;
  if (!creds) return NextResponse.next();
  const { pathname } = req.nextUrl;
  if (pathname.startsWith("/api/ingest/") || pathname === "/api/health") return NextResponse.next();

  const header = req.headers.get("authorization") ?? "";
  if (header.startsWith("Basic ")) {
    try {
      if (atob(header.slice(6)) === creds) return NextResponse.next();
    } catch {
      /* fall through */
    }
  }
  return new NextResponse("Authentication required", {
    status: 401,
    headers: { "WWW-Authenticate": 'Basic realm="Wisdom Ops"' },
  });
}

export const config = {
  matcher: ["/((?!_next/static|_next/image|favicon.ico).*)"],
};
