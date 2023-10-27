import { start } from '../output/Main/index.js'

setInterval(() => {
    new CustomEvent("tick", { time: new Date().toISOString() })
}, 1000)

start({ initialCount: 100})()
