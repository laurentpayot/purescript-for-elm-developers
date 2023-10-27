import { start } from '../output/Main/index.js'

setInterval(() => {
    document.body.dispatchEvent(new CustomEvent("tick",{ detail: JSON.stringify(
        new Date().toISOString())
    }))
}, 1000)

// document.body.addEventListener("tick", ({detail}) => console.log("payload:", JSON.parse(detail)), false)

start({ initialCount: 100})()
