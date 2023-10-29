import { start } from '../output/Main/index.js'

start({ initialCount: 100})()

setInterval(() => {
    document.dispatchEvent(new CustomEvent("time",{ detail: JSON.stringify(
        { time: new Date().toISOString()
        , foo: "bar"
        }
    )}))
}, 1000)

// document.addEventListener("time", ({detail}) => console.log(JSON.parse(detail)), false)
