import { defineConfig } from 'vite'

export default defineConfig({
  root: "src",
  server: {
    port: 5174
  },
  build: {
    outDir: "../output",
    emptyOutDir: true,
    sourcemap: true
  }
});
