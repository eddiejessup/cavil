// Types.

import { ClientError, fetchCavil } from "./Common";

export type CaseLabel = string;

export type CaseId = string;

export type DecisionId = string;

export type Variant = number;

export type NrVariants = number;

export interface CaseSummary {
  id: CaseId;
  nextDecisionId: DecisionId;
  label: CaseLabel;
  nrVariants: NrVariants;
  decisions: Array<DecisionSummary>;
}

export interface DecisionSummary {
  id: DecisionId;
  decisionTime: string;
  variant: Variant;
  isValid: boolean;
  invalidationReason?: string;
}

// Operations.

export const caseCreate = async (
  caseLabel: CaseLabel,
  nrVariants: NrVariants,
  onSuccess: (id: CaseId) => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `case`,
    { bodyObj: { nrVariants, label: caseLabel }, method: "PUT" },
    onSuccess,
    onClientError,
    onOtherError
  );

export const caseSummarise = async (
  caseId: CaseId,
  onSuccess: (v: CaseSummary) => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `case/${caseId}`,
    {},
    async (res) => {
      const caseSummary = await res.json();
      onSuccess(caseSummary);
    },
    onClientError,
    onOtherError
  );

export const caseDecide = async (
  caseId: CaseId,
  decisionId: DecisionId,
  onSuccess: (v: Variant) => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `case/${caseId}/${decisionId}`,
    { method: "PUT" },
    async (res) => {
      const variant = await res.json();
      onSuccess(variant);
    },
    onClientError,
    onOtherError
  );

export const caseDecisionInvalidate = async (
  caseId: CaseId,
  decisionId: DecisionId,
  reason: string,
  onSuccess: () => void,
  onClientError: (err: ClientError) => void,
  onOtherError: (err: Error) => void
) =>
  await fetchCavil(
    `case/${caseId}/${decisionId}/invalidate`,
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
