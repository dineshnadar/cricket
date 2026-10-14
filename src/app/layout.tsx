import type { Metadata, Viewport } from "next";
import "@fontsource/fira-sans/400.css";
import "@fontsource/fira-sans/500.css";
import "@fontsource/fira-sans/600.css";
import "@fontsource/fira-code/400.css";
import "@fontsource/fira-code/600.css";
import "./globals.css";
import Sidebar from "@/components/Sidebar";

export const metadata: Metadata = {
  title: { default: "Wisdom Ops", template: "%s · Wisdom Ops" },
  description: "Change intelligence for the repos behind your module",
};

export const viewport: Viewport = {
  width: "device-width",
  initialScale: 1,
  themeColor: [{ color: "#020617" }],
};

// Runs before paint so the saved theme never flashes. Dark is the default.
const themeScript = `try{var t=localStorage.getItem("wo-theme");if(t==="light")document.documentElement.dataset.theme="light"}catch(e){}`;

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en" suppressHydrationWarning>
      <head>
        <script dangerouslySetInnerHTML={{ __html: themeScript }} />
      </head>
      <body>
        <a className="skip-link" href="#main">
          Skip to content
        </a>
        <div className="shell">
          <Sidebar />
          <main className="main" id="main" tabIndex={-1}>
            {children}
          </main>
        </div>
      </body>
    </html>
  );
}
