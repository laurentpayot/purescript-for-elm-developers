export const multiply = a => b =>
    a * b

export const catBase64JS = text => fontSize =>
    //  Wrapping in a PS Effect with the "thunk" `() =>` is needed so function is not considered pure and is run every time
    async () => {
        const response = await fetch(`https://cataas.com/cat/says/${
            encodeURIComponent(text)
        }?filter=mono&fontColor=red&fontSize=${
            fontSize
        }&type=square`)
        const array = await response.body?.getReader().read()
        return btoa(String.fromCharCode.apply(null, array?.value))
    }
