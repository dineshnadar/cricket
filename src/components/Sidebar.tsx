"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { useEffect, useState } from "react";
import { Bot, HeartPulse, LayoutDashboard, MessageSquareText, Moon, Plus, Sparkles, Sun } from "lucide-react";

const links = [
  { href: "/chat", label: "Assistant", icon: MessageSquareText, match: (p: string) => p.startsWith("/chat") },
  { href: "/", label: "Dashboard", icon: LayoutDashboard, match: (p: string) => p === "/" || p.startsWith("/changes") || p.startsWith("/incidents") },
  { href: "/agents", label: "Agents", icon: Bot, match: (p: string) => p === "/agents" || /^\/agents\/(?!new)/.test(p) },
  { href: "/agents/new", label: "Create agent", icon: Plus, match: (p: string) => p === "/agents/new" },
];

function ThemeToggle() {
  const [light, setLight] = useState(false);
  useEffect(() => setLight(document.documentElement.dataset.theme === "light"), []);
  const toggle = () => {
    const next = !light;
    setLight(next);
    if (next) document.documentElement.dataset.theme = "light";
    else delete document.documentElement.dataset.theme;
    try {
      localStorage.setItem("wo-theme", next ? "light" : "dark");
    } catch {
      /* storage unavailable */
    }
  };
  return (
    <button className="btn ghost" onClick={toggle} aria-label={light ? "Switch to dark theme" : "Switch to light theme"} title="Toggle theme">
      {light ? <Moon size={16} aria-hidden /> : <Sun size={16} aria-hidden />}
      <span className="health-text">{light ? "Dark theme" : "Light theme"}</span>
    </button>
  );
}

export default function Sidebar() {
  const pathname = usePathname() ?? "/";
  return (
    <aside className="side" aria-label="Primary">
      <div className="brand">
        <span className="brand-mark" aria-hidden>
          <Sparkles size={16} strokeWidth={2.25} />
        </span>
        <span>
          Wisdom Ops
          <small>Change intelligence</small>
        </span>
      </div>
      <nav className="nav">
        <div className="nav-label">Workspace</div>
        {links.map(({ href, label, icon: Icon, match }) => (
          <Link key={href} href={href} aria-current={match(pathname) ? "page" : undefined} title={label}>
            <Icon size={18} aria-hidden />
            <span>{label}</span>
          </Link>
        ))}
      </nav>
      <div className="side-foot">
        <ThemeToggle />
        <a className="btn ghost" href="/api/health" target="_blank" rel="noreferrer" title="Service health">
          <HeartPulse size={16} aria-hidden />
          <span className="health-text">Health</span>
        </a>
      </div>
    </aside>
  );
}
