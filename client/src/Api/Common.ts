declare const API_BASE_URL: string;

export interface ClientError {
  errorType: string;
  errorDetail: string;
}

export const renderClientError = (err: ClientError) =>
  `${err.errorType}: ${err.errorDetail}`;

export const fallBackErrorMsg = "Something went wrong";

interface FetchJSON {
  bodyObj?: any;
  method?: string;
}

export const setAuth = (username: string, password: string) => {
  window.localStorage.setItem("cavil/username", username);
  window.localStorage.setItem("cavil/password", password);
};

const fetchJSON = async (url: string, opts: FetchJSON) => {
  const username = window.localStorage.getItem("cavil/username");
  const password = window.localStorage.getItem("cavil/password");
  const headers = {
    "Content-Type": "application/json",
    Authorization: "Basic " + btoa(`${username}:${password}`),
  };
  return fetch(url, {
    ...opts,
    headers,
    body: opts.bodyObj === undefined ? undefined : JSON.stringify(opts.bodyObj),
  });
};

export const fetchCavil = async (
  path: string,
  opts: FetchJSON,
  onSuccess: (res: any) => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) => {
  try {
    const res = await fetchJSON(`${API_BASE_URL}/${path}`, opts);
    if (res.status === 200) {
      onSuccess(res);
    } else if ([400, 500].includes(res.status)) {
      const errObj = await res.json();
      onClientError(errObj);
    } else {
      throw new Error("Unknown error");
    }
  } catch (error) {
    // Try to decode error as ClientError, and handle that. Otherwise throw.
    try {
      const errObj = await error.json();
      onClientError(errObj);
    } catch (_) {
      onOtherError(error);
    }
  }
};
