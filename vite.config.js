import { defineConfig } from "vite"

// stolen from https://github.com/seastian/purescript-vite-lazy
const purescriptPlugin = {
    name: "dynamic-imports",
    transform(src, currentFile) {
        let moduleImportsToRemove = ["DynamicImport"]
        const withDynamicImports = src.replaceAll(
            /\((DynamicImport\.dynamicImport\(([A-Z]\w*)\.([a-z_]\w*)\))\)/g,
            (match, g, moduleName, componentName) => {
                moduleImportsToRemove.push(moduleName)
                const modulePath = `../${moduleName.replace("_", ".")}/index.js`
                return `(() => import("${modulePath}").then(r => r.${componentName}))`
            }
        )
        const withStaticImportsRemoved = moduleImportsToRemove.reduce(
            (transformed, moduleName) => transformed.replaceAll(new RegExp(`import \\* as ${moduleName} from (.*?);`, "g"), ""),
            withDynamicImports
        )
        const checkIfModulesAreIsolated =
            moduleImportsToRemove.flatMap(moduleName => withStaticImportsRemoved.includes(`${moduleName}.`) ? [moduleName] : [])
        checkIfModulesAreIsolated.map(moduleName => this.error(`${currentFile} contains references to ${moduleName}.`))
        return { code: withStaticImportsRemoved }
    }
}

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

        minify: 'terser', // using terser as esbuild does not support pure functions optimizations for elm https://github.com/evanw/esbuild/issues/731#issuecomment-770564592
        terserOptions: {
            format: {
                comments: false
            },
            compress: {
                // pure_funcs: ['F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9'],
                pure_getters: true,
                keep_fargs: false,
                unsafe_comps: true,
                unsafe: true,
                passes: 2,
            },
            mangle: true
        },

    },
    plugins: [
        purescriptPlugin
    ]
})
