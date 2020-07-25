import React from "react";
import { Container, Grid, Chip } from "@material-ui/core";
import { Alert } from "@material-ui/lab";
import FormatListNumberedIcon from "@material-ui/icons/FormatListNumbered";
import FingerprintIcon from "@material-ui/icons/Fingerprint";
import {
  CaseSummary,
  ClientError,
  renderClientError,
  fallBackErrorMsg,
  caseSummarise,
} from "../../Api";
import { useStyles } from "../Style";
import {
  Title,
  FetchState,
  ErrorMsg,
  notFetched,
  fetchError,
  fetchSuccess,
} from "../Common";
import { NewDecision } from "./NewDecision";
import { DecisionList } from "./DecisionList";
import { useParams } from "react-router-dom";

interface CaseScreenProps {}

export const CaseScreen: React.FunctionComponent<CaseScreenProps> = (props) => {
  const { caseId } = useParams();
  const classes = useStyles();

  const [fetchedCaseSummary, setFetchedCaseSummary] = React.useState<
    FetchState<CaseSummary>
  >(notFetched());

  const onCaseChanged = () => {
    setFetchedCaseSummary(notFetched());
  };

  React.useEffect(() => {
    const fetchData = async () => {
      if (fetchedCaseSummary.kind === "notFetched") {
        await caseSummarise(
          caseId,
          (value: CaseSummary) => {
            setFetchedCaseSummary(fetchSuccess(value));
          },
          (errObj: ClientError) => {
            setFetchedCaseSummary(
              fetchError(renderClientError(errObj) as ErrorMsg)
            );
          },
          (_err: Error) => {
            setFetchedCaseSummary(fetchError(fallBackErrorMsg as ErrorMsg));
          }
        );
      }
    };

    fetchData();
  }, [caseId, fetchedCaseSummary]);

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        {caseId && (
          <Grid item xs={12}>
            {
              // Title with chip.
            }
            <Grid container spacing={1}>
              <Grid item>
                <Title>
                  Case{" "}
                  {fetchedCaseSummary.kind === "fetchSuccess"
                    ? fetchedCaseSummary.value.label
                    : "?"}
                </Title>
              </Grid>
              {fetchedCaseSummary.kind === "fetchSuccess" && (
                <Grid item>
                  <Chip
                    icon={<FormatListNumberedIcon />}
                    label={`${fetchedCaseSummary.value.nrVariants} variants`}
                    color="primary"
                    size="small"
                  />
                </Grid>
              )}
              <Grid item>
                <Chip
                  icon={<FingerprintIcon />}
                  label={caseId}
                  color="secondary"
                  size="small"
                />
              </Grid>
            </Grid>
          </Grid>
        )}

        {fetchedCaseSummary.kind === "fetchError" && (
          <Grid item xs={12}>
            <Alert severity="error">
              Couldn't get case summary: {fetchedCaseSummary.errMsg}
            </Alert>
          </Grid>
        )}

        <Grid item xs={12}>
          <NewDecision
            fetchedCaseSummary={fetchedCaseSummary}
            onCaseChanged={onCaseChanged}
          />
        </Grid>

        <Grid item xs={12}>
          <DecisionList
            fetchedCaseSummary={fetchedCaseSummary}
            onCaseChanged={onCaseChanged}
          />
        </Grid>
      </Grid>
    </Container>
  );
};
