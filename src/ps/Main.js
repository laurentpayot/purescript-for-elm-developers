export const multiply = a => b => a * b

export const catBase64JS = async (text) => {
    console.log("*** text = ", text)
    const response = await fetch(`https://cataas.com/cat/says/${text}?fontSize=50&fontColor=red`)
    const array = await response.body.getReader().read()
    const base64 = btoa(String.fromCharCode.apply(null, array.value))
    return base64
}
