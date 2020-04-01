// module FFI.Matelialize
const M = require('materialize-css/dist/js/materialize')

exports.initModalJS = element => (error, success) => {
    M.Modal.init(element, { startingTop: '20%', endingTop: '20%' })
    success()
    return (cancelError, cancelerError, cancelerSuccess) => {
        cancelerSuccess()
    }
}

exports.showModalJS = element => (error, success) => {
    M.Modal.getInstance(element).open()
    success()
    return (cancelError, cancelerError, cancelerSuccess) => {
        cancelerSuccess()
    }
}
