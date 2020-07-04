// Types.

export type CaseLabel = string;

export type DecisionToken = string;

export type Variant = number;

export interface CaseSummary {
  nextDecisionToken: DecisionToken;
  label: string;
  nrVariants: number;
  decisions: Array<DecisionSummary>;
}

export interface DecisionSummary {
  token: DecisionToken;
  decisionTimeUTC: string;
  variant: number;
  isValid: boolean;
  invalidationReason ?: string;
}

export interface ClientError {
  errorType: string;
  errorDetail: string;
}

export const renderClientError = (err: ClientError) =>
  `${err.errorType}: ${err.errorDetail}`;

export const fallBackErrorMsg = "Something went wrong";

// Operations.

export const caseCreate = async (
  caseLabel: CaseLabel,
  nrVariants: number,
  onSuccess: () => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `/case/${caseLabel}`,
    { bodyObj: { nrVariants }, method: "PUT" },
    onSuccess,
    onClientError,
    onOtherError
  );

export const caseSummarise = async (
  caseLabel: CaseLabel,
  onSuccess: (v: CaseSummary) => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `/case/${caseLabel}`,
    {},
    async (res) => {
      const caseSummary = await res.json();
      onSuccess(caseSummary);
    },
    onClientError,
    onOtherError
  );

export const caseDecide = async (
  caseLabel: CaseLabel,
  decisionToken: DecisionToken,
  onSuccess: (v: Variant) => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `/case/${caseLabel}/${decisionToken}`,
    { method: "PUT" },
    async (res) => {
      const variant = await res.json();
      onSuccess(variant);
    },
    onClientError,
    onOtherError
  );

export const caseDecisionInvalidate = async (
  caseLabel: CaseLabel,
  decisionToken: DecisionToken,
  reason: string,
  onSuccess: () => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `/case/${caseLabel}/${decisionToken}/invalidate`,
    { bodyObj: { reason }, method: "POST" },
    onSuccess,
    onClientError,
    onOtherError
  );

export const casesSummarise = async (
  onSuccess: (v: Array<CaseSummary>) => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) => {
  await fetchCavil(
    "/case",
    {},
    async (res) => {
      const caseSummaries = await res.json();
      onSuccess(caseSummaries);
    },
    onClientError,
    onOtherError
  );
};

// Helpers.

interface FetchJSON {
  bodyObj?: any;
  method?: string;
}

const fetchJSON = async (url: string, opts: FetchJSON) => {
  const username = window.localStorage.getItem('cavil/username')
  const password = window.localStorage.getItem('cavil/password')
  const headers = {
    "Content-Type": "application/json",
    "Authorization": 'Basic ' + btoa(`${username}:${password}`),
  }
  return fetch(url, {
    ...opts,
    headers,
    body: opts.bodyObj === undefined ? undefined : JSON.stringify(opts.bodyObj),
  });
}

const fetchCavil = async (
  url: string,
  opts: FetchJSON,
  onSuccess: (res: any) => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) => {
  try {
    const res = await fetchJSON(url, opts);
    if (res.status === 200) {
      onSuccess(res);
    } else if (res.status === 400) {
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
