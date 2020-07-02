export type CaseLabel = string;

export type DecisionToken = string;

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
}
