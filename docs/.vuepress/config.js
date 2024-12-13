import { defineUserConfig } from "@vuepress/cli";
import { defaultTheme } from "@vuepress/theme-default";

export default defineUserConfig({
  lang: "en-US",
  title: "Pilot Buzz",
  description: "Documentation for the Pilot Buzz programming language",
  theme: defaultTheme({
    repo: "aldrin-labs/buzz",
    docsDir: "docs",
    editLink: true,
    navbar: [
      { text: "Guide", link: "/" },
      { text: "Reference", link: "/api/" }
    ],
    sidebar: "auto"
  })
})
