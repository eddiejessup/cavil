import rewire from "rewire"
const Common = rewire("./Common")
const fetchJSON = Common.__get__("fetchJSON")
// @ponicode
describe("Common.renderClientError", () => {
    test("0", () => {
        let callFunction: any = () => {
            Common.renderClientError({ errorType: "day", errorDetail: "Dwarf Crocodile" })
        }
    
        expect(callFunction).not.toThrow()
    })

    test("1", () => {
        let callFunction: any = () => {
            Common.renderClientError({ errorType: "minute", errorDetail: "Dwarf Crocodile" })
        }
    
        expect(callFunction).not.toThrow()
    })

    test("2", () => {
        let callFunction: any = () => {
            Common.renderClientError({ errorType: "quarter", errorDetail: "Spectacled Caiman" })
        }
    
        expect(callFunction).not.toThrow()
    })

    test("3", () => {
        let callFunction: any = () => {
            Common.renderClientError({ errorType: "second", errorDetail: "Dwarf Crocodile" })
        }
    
        expect(callFunction).not.toThrow()
    })

    test("4", () => {
        let callFunction: any = () => {
            Common.renderClientError({ errorType: "minute", errorDetail: "Nile Crocodile" })
        }
    
        expect(callFunction).not.toThrow()
    })

    test("5", () => {
        let callFunction: any = () => {
            Common.renderClientError({ errorType: "", errorDetail: "" })
        }
    
        expect(callFunction).not.toThrow()
    })
})

// @ponicode
describe("fetchJSON", () => {
    test("0", async () => {
        await fetchJSON("http://base.com", { bodyObj: "a1969970175", method: "POST" })
    })

    test("1", async () => {
        await fetchJSON("https://api.telegram.org/", { bodyObj: "data:image/svg+xml;charset=UTF-8,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20baseProfile%3D%22full%22%20width%3D%22undefined%22%20height%3D%22undefined%22%3E%3Crect%20width%3D%22100%25%22%20height%3D%22100%25%22%20fill%3D%22grey%22%2F%3E%3Ctext%20x%3D%22NaN%22%20y%3D%22NaN%22%20font-size%3D%2220%22%20alignment-baseline%3D%22middle%22%20text-anchor%3D%22middle%22%20fill%3D%22white%22%3Eundefinedxundefined%3C%2Ftext%3E%3C%2Fsvg%3E", method: "POST" })
    })

    test("2", async () => {
        await fetchJSON("http://www.example.com/route/123?foo=bar", { bodyObj: true, method: "POST" })
    })

    test("3", async () => {
        await fetchJSON("http://example.com/showcalendar.html?token=CKF50YzIHxCTKMAg", { bodyObj: true, method: "POST" })
    })

    test("4", async () => {
        await fetchJSON("https://api.telegram.org/", { bodyObj: 12, method: "POST" })
    })

    test("5", async () => {
        await fetchJSON("", { bodyObj: NaN, method: "" })
    })
})

// @ponicode
describe("Common.fetchCavil", () => {
    test("0", async () => {
        await Common.fetchCavil("C:\\\\path\\to\\folder\\", { bodyObj: "data:image/svg+xml;charset=UTF-8,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20version%3D%221.1%22%20baseProfile%3D%22full%22%20width%3D%22undefined%22%20height%3D%22undefined%22%3E%3Crect%20width%3D%22100%25%22%20height%3D%22100%25%22%20fill%3D%22grey%22%2F%3E%3Ctext%20x%3D%22NaN%22%20y%3D%22NaN%22%20font-size%3D%2220%22%20alignment-baseline%3D%22middle%22%20text-anchor%3D%22middle%22%20fill%3D%22white%22%3Eundefinedxundefined%3C%2Ftext%3E%3C%2Fsvg%3E", method: "POST" }, () => undefined, () => undefined, () => undefined)
    })

    test("1", async () => {
        await Common.fetchCavil(".", { bodyObj: true, method: "DELETE" }, () => undefined, () => undefined, () => undefined)
    })

    test("2", async () => {
        await Common.fetchCavil("/path/to/file", { bodyObj: 987650, method: "POST" }, () => undefined, () => undefined, () => undefined)
    })

    test("3", async () => {
        await Common.fetchCavil("path/to/file.ext", { bodyObj: 56784, method: "POST" }, () => undefined, () => undefined, () => undefined)
    })

    test("4", async () => {
        await Common.fetchCavil("/path/to/file", { bodyObj: true, method: "POST" }, () => undefined, () => undefined, () => undefined)
    })

    test("5", async () => {
        await Common.fetchCavil("", { bodyObj: "", method: "" }, () => undefined, () => undefined, () => undefined)
    })
})
