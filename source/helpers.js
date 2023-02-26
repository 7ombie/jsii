export const iife = lambda => lambda();
export const report = console.error;
export const put = console.log;
export const not = arg => !arg;

window.put = put;