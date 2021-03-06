"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.measurePromise = exports.hrTimeToMs = exports.elapsedTimeMsFrom = exports.measurePromiseExecution = exports.measureExecution = void 0;
function measureExecution(fn) {
    const start = process.hrtime();
    const r = fn();
    const elapsedTimeMs = hrTimeToMs(process.hrtime(start));
    return {
        elapsedTimeMs,
        r
    };
}
exports.measureExecution = measureExecution;
async function measurePromiseExecution(fn) {
    try {
        const start = process.hrtime();
        const r = await fn();
        const elapsedTimeMs = hrTimeToMs(process.hrtime(start));
        return {
            elapsedTimeMs,
            r
        };
    }
    catch (e) {
        throw e;
    }
}
exports.measurePromiseExecution = measurePromiseExecution;
function elapsedTimeMsFrom(relativeTo) {
    return hrTimeToMs(process.hrtime(relativeTo));
}
exports.elapsedTimeMsFrom = elapsedTimeMsFrom;
function hrTimeToMs(hrTime) {
    return hrTime[0] * 1.0e3 + hrTime[1] * 1.0e-6;
}
exports.hrTimeToMs = hrTimeToMs;
async function measurePromise(fn) {
    const start = process.hrtime();
    const r = await fn();
    const elapsedTimeMs = hrTimeToMs(process.hrtime(start));
    return {
        elapsedTimeMs,
        r
    };
}
exports.measurePromise = measurePromise;
//# sourceMappingURL=timer.js.map