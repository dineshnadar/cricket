import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  output: "standalone",
  serverExternalPackages: ["mongodb", "nodemailer"],
};

export default nextConfig;
