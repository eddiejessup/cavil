import React from "react";
import { Grid, Paper, CircularProgress, Chip, Button } from "@material-ui/core";
import { Alert } from "@material-ui/lab";
import {
  CaseSummary,
  ClientError,
  renderClientError,
  fallBackErrorMsg,
  caseDecide,
} from "./Api";
import { useStyles } from "./Style";
import { SubTitle } from "./Common";

export interface NewDecisionProps {
  caseSummary?: CaseSummary;
}

export const NewDecision: React.FunctionComponent<NewDecisionProps> = ({
  caseSummary,
}) => {
  const classes = useStyles();

  const [error, setError] = React.useState<string | null>(null);

  const onClick = async () => {
    if (caseSummary === undefined) {
      setError("No case information available");
    } else {
      try {
        await caseDecide(
          caseSummary.label,
          caseSummary.nextDecisionToken,
          (_var) => {
            window.location.reload();
          },
          (errObj: ClientError) => {
            setError(renderClientError(errObj));
          }
        );
      } catch (error) {
        setError(fallBackErrorMsg);
      }
    }
  };

  const content =
    caseSummary === undefined ? (
      <CircularProgress />
    ) : (
      <Button variant="contained" color="primary" onClick={onClick}>
        Go
      </Button>
    );

  return (
    <Paper className={classes.contentPaper}>
      <Grid container spacing={2} direction="column">
        <Grid item container direction="row" spacing={1}>
          <Grid item>
            <SubTitle>Make decision</SubTitle>
          </Grid>
          {caseSummary && (
            <Grid item>
              <Chip
                label={caseSummary.nextDecisionToken}
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
