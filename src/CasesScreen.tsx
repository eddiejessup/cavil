import React from "react";
import {
  Container,
  Grid,
  Paper,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  CircularProgress,
  Link as MaterialLink,
} from "@material-ui/core";
import { Alert } from "@material-ui/lab";
import {
  CaseSummary,
  ClientError,
  renderClientError,
  fallBackErrorMsg,
  casesSummarise,
} from "./Api";
import { NewCaseForm } from "./NewCase";
import { useStyles } from "./Style";
import {
  Title,
  SubTitle,
  FetchState,
  ErrorMsg,
  notFetched,
  fetchError,
  fetchSuccess,
} from "./Common";
import { Link as RouterLink, useRouteMatch } from "react-router-dom";

interface CasesScreenProps {}

type CaseSummaries = Array<CaseSummary>;

export const CasesScreen: React.FunctionComponent<CasesScreenProps> = (
  props
) => {
  const classes = useStyles();

  const [fetchedCaseSummaries, setFetchedCaseSummaries] = React.useState<
    FetchState<CaseSummaries>
  >(notFetched());

  const onCasesChanged = () => {
    setFetchedCaseSummaries(notFetched());
  };

  React.useEffect(() => {
    const fetchData = async () => {
      if (fetchedCaseSummaries.kind === "notFetched") {
        await casesSummarise(
          (value: CaseSummaries) => {
            setFetchedCaseSummaries(fetchSuccess(value));
          },
          (errObj: ClientError) => {
            setFetchedCaseSummaries(
              fetchError(renderClientError(errObj) as ErrorMsg)
            );
          },
          (_err: Error) => {
            setFetchedCaseSummaries(fetchError(fallBackErrorMsg as ErrorMsg));
          }
        );
      }
    };

    fetchData();
  }, [fetchedCaseSummaries]);

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        <Grid item xs={12}>
          <Title>Cases</Title>
        </Grid>

        <Grid item xs={12}>
          <Paper className={classes.contentPaper}>
            <Grid container spacing={2} direction="column">
              <Grid item>
                <SubTitle>Existing cases</SubTitle>
              </Grid>

              {fetchedCaseSummaries.kind === "fetchError" && (
                <Grid item>
                  <Alert severity="error">
                    Couldn't get cases: {fetchedCaseSummaries.errMsg}
                  </Alert>
                </Grid>
              )}

              {fetchedCaseSummaries.kind === "notFetched" && (
                <Grid item>
                  <CircularProgress />
                </Grid>
              )}

              {fetchedCaseSummaries.kind === "fetchSuccess" && (
                <Grid item>
                  <CaseSummaries caseSummaries={fetchedCaseSummaries.value} />
                </Grid>
              )}
            </Grid>
          </Paper>
        </Grid>

        <Grid item xs={12}>
          <Paper className={classes.contentPaper}>
            <Grid container spacing={2} direction="column">
              <Grid item>
                <SubTitle>New case</SubTitle>
              </Grid>

              <Grid item>
                <NewCaseForm onCasesChanged={onCasesChanged} />
              </Grid>
            </Grid>
          </Paper>
        </Grid>
      </Grid>
    </Container>
  );
};

interface CaseSummariesProps {
  caseSummaries: Array<CaseSummary>;
}

const CaseSummaries: React.FunctionComponent<CaseSummariesProps> = (props) => {
  const { url } = useRouteMatch();

  return (
    <Table size="small">
      <TableHead>
        <TableRow>
          <TableCell>Label</TableCell>
          <TableCell>Number of variants</TableCell>
          <TableCell>Number of decisions</TableCell>
          <TableCell>Last decision (UTC)</TableCell>
          <TableCell>Last decision variant</TableCell>
        </TableRow>
      </TableHead>
      <TableBody>
        {props.caseSummaries.map((caseSummary) => {
          const validDecisions = caseSummary.decisions.filter((d) => d.isValid);
          const nrValidDecisions = validDecisions.length;
          const lastValidDecision = validDecisions[nrValidDecisions - 1];
          return (
            <TableRow key={caseSummary.label}>
              <TableCell>
                <MaterialLink
                  component={RouterLink}
                  to={`${url}/${caseSummary.label}`}
                >
                  {caseSummary.label}
                </MaterialLink>
              </TableCell>
              <TableCell>{caseSummary.nrVariants}</TableCell>
              <TableCell>{nrValidDecisions}</TableCell>
              <TableCell>
                {lastValidDecision ? lastValidDecision.decisionTimeUTC : ""}
              </TableCell>
              <TableCell>
                {lastValidDecision ? lastValidDecision.variant : ""}
              </TableCell>
            </TableRow>
          );
        })}
      </TableBody>
    </Table>
  );
};
