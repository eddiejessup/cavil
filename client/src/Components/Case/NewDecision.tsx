import React from "react";
import { Grid, Paper, CircularProgress, Chip, Button } from "@material-ui/core";
import FingerprintIcon from "@material-ui/icons/Fingerprint";
import { Alert } from "@material-ui/lab";
import {
  CaseSummary,
  ClientError,
  renderClientError,
  fallBackErrorMsg,
  caseDecide,
} from "../../Api";
import { useStyles } from "../Style";
import { SubTitle, FetchState } from "../Common";

export interface NewDecisionProps {
  fetchedCaseSummary: FetchState<CaseSummary>;
  onCaseChanged: () => void;
}

export const NewDecision: React.FunctionComponent<NewDecisionProps> = ({
  fetchedCaseSummary,
  onCaseChanged,
}) => {
  const classes = useStyles();

  const [error, setError] = React.useState<string | null>(null);
  const [pending, setPending] = React.useState<boolean>(false);

  const onClick = async () => {
    if (fetchedCaseSummary.kind === "fetchSuccess") {
      setPending(true);
      setError(null);
      await caseDecide(
        fetchedCaseSummary.value.id,
        fetchedCaseSummary.value.nextDecisionId,
        (_variant) => {
          onCaseChanged();
        },
        (errObj: ClientError) => {
          setError(renderClientError(errObj));
        },
        (_err: Error) => {
          setError(fallBackErrorMsg);
        }
      );
      setPending(false);
    }
  };

  let content;
  switch (fetchedCaseSummary.kind) {
    case "notFetched":
      content = <CircularProgress />;
      break;
    case "fetchError":
      content = <Alert severity="error">{fetchedCaseSummary.errMsg}</Alert>;
      break;
    case "fetchSuccess":
      content = (
        <Button
          variant="contained"
          color="primary"
          onClick={onClick}
          disabled={pending}
        >
          Go
        </Button>
      );
  }

  return (
    <Paper className={classes.contentPaper}>
      <Grid container spacing={2} direction="column">
        <Grid item container direction="row" spacing={1}>
          <Grid item>
            <SubTitle>Make decision</SubTitle>
          </Grid>
          {fetchedCaseSummary.kind === "fetchSuccess" && (
            <Grid item>
              <Chip
                icon={<FingerprintIcon />}
                label={fetchedCaseSummary.value.nextDecisionId}
                color="secondary"
                size="small"
              />
            </Grid>
          )}
        </Grid>

        {error && (
          <Grid item>
            <Alert severity="error">{error}</Alert>
          </Grid>
        )}

        <Grid item>{content}</Grid>
      </Grid>
    </Paper>
  );
};
