export const multiply = a => b => a * b

export const catBase64_ = text => (onError, onSuccess) => {
    // not awaiting fetch but returning cancel function
    fetch(`https://cataas.com/cat/says/${text}?fontSize=50&fontColor=red`)
    .then(async response => {
        const reader = await response.getReader()
        const array = await reader.read()
        const base64 = btoa(String.fromCharCode.apply(null, array.value))
        onSuccess(base64)
    })
    return (cancelError, onCancelerError, onCancelerSuccess) => {
        console.log("Cat canceled!")
        onCancelerSuccess()
    }
}
