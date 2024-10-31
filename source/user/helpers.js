export const JS = {
    keys: name => `${name}?.ƥkeys?.() ?? ƥkeys(${name} ?? [])`,
    values: name => `${name}?.ƥvalues?.() ?? Object.values(${name} ?? [])`,
    invert: expression => `!(${expression})`,
}
