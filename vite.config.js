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
    },
    plugins: [
        purescriptPlugin
    ]
})
