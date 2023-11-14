import { defineConfig } from "vite"

export default defineConfig({
    // clearScreen: false,
    server: {
        port: 5000,
        strictPort: true
    },
    root: './src',
    publicDir: '../public', // relative to viteConfig.root directory
    build: {
        outDir: '../dist',
        emptyOutDir: true,
        target: 'es2022', // ES2022 allowed for terser v5.16+ https://github.com/vitejs/vite/pull/12197
        minify: 'terser',
        terserOptions: {
            format: {
                comments: false
            },
            compress: {
                pure_getters: true,
                keep_fargs: false,
                unsafe: true,
                unsafe_arrows: true,
                unsafe_comps: true,
                unsafe_Function: true,
                unsafe_math: true,
                unsafe_symbols: true,
                unsafe_methods: true,
                unsafe_proto: true,
                unsafe_undefined: true,
                passes: 3,
            },
            // mangle: {
            //     properties: 'keep_quoted', // effective but dangerous?
            // },
            mangle: true
        },
    },
    plugins: []
})
