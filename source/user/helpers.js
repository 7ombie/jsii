export const JS = {
    keys: name => `${name}?.ƥkeys?.() ?? Object.keys(${name} ?? [])`,
    values: name => `${name}?.ƥvalues?.() ?? Object.values(${name} ?? [])`,
    invert: expression => `!(${expression})`,
}
