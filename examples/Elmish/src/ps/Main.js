export const multiply = a => b =>
    a * b

export const catBase64JS = text => fontSize =>
    //  Wrapping in a PS Effect with the "thunk" `() =>` is needed so function is not considered pure and is run every time
    async () => {
        const response = await fetch(`https://cataas.com/cat/says/${text}?fontSize=${fontSize}&fontColor=red`)
        const array = await response.body?.getReader().read()
        return btoa(String.fromCharCode.apply(null, array?.value))
    }
