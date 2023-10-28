import { start } from '../output/Main/index.js'

start({ initialCount: 100})()

setInterval(() => {
    document.dispatchEvent(new CustomEvent("tick",{ detail: JSON.stringify(
        { time: new Date().toISOString() }
    )}))
}, 1000)

document.addEventListener("tick", ({detail}) => console.log("payload:", JSON.parse(detail)), false)
