export const multiply = a => b => a * b

export const catBase64JS = text => async fontSize => {
    const response = await fetch(`https://cataas.com/cat/says/${text}?fontSize=${fontSize}&fontColor=red`)
    const array = await response.body.getReader().read()
    return btoa(String.fromCharCode.apply(null, array.value))
}
