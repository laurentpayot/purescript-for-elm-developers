import { main as start } from '../output/Main/index.js'

start({ initialCount: 100})

function sendPS(event, payload) {
    return document.dispatchEvent(new CustomEvent(event, { detail: JSON.stringify(payload) }))
}

setInterval(() => {
    sendPS("time",{
        time: new Date().toISOString(),
        foo: "bar"
    })
}, 1000)

// document.addEventListener("time", ({detail}) => console.log(JSON.parse(detail)), false)
