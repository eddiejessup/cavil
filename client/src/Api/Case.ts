// Types.

import { ClientError, fetchCavil } from "./Common";

export type CaseLabel = string;

export type DecisionToken = string;

export type Variant = number;

export interface CaseSummary {
  nextDecisionToken: DecisionToken;
  label: CaseLabel;
  nrVariants: number;
  decisions: Array<DecisionSummary>;
}

export interface DecisionSummary {
  token: DecisionToken;
  decisionTime: string;
  variant: number;
  isValid: boolean;
  invalidationReason?: string;
}

// Operations.

export const caseCreate = async (
  caseLabel: CaseLabel,
  nrVariants: number,
  onSuccess: () => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `case/${caseLabel}`,
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
    `case/${caseLabel}`,
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
    `case/${caseLabel}/${decisionToken}`,
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
    `case/${caseLabel}/${decisionToken}/invalidate`,
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
    `case`,
    {},
    async (res) => {
      const caseSummaries = await res.json();
      onSuccess(caseSummaries);
    },
    onClientError,
    onOtherError
  );
};
