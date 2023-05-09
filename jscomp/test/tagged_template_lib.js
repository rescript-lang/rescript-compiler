exports.sql = (strings, ...values) => {
    let result = "";
    for (let i = 0; i < values.length; i++) {
        result += strings[i] + values[i];
    }
    result += strings[values.length];
    return result;
};