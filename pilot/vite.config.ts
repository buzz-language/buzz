import path from "path"
import react from "@vitejs/plugin-react"
import { defineConfig } from "vite"

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: [
      {
        find: '@common/utilities',
        replacement: path.resolve(__dirname, 'src/common/utilities.ts')
      },
      {
        find: '@',
        replacement: path.resolve(__dirname, 'src')
      },
      {
        find: '@common',
        replacement: path.resolve(__dirname, 'src/common')
      },
      {
        find: '@components',
        replacement: path.resolve(__dirname, 'src/components')
      },
      {
        find: '@modules',
        replacement: path.resolve(__dirname, 'src/modules')
      }
    ]
  },
  build: {
    target: 'esnext',
    sourcemap: true
  }
})

