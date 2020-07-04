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
import { Title } from "./Common";
import { Link as RouterLink, useRouteMatch } from "react-router-dom";

interface CasesScreenProps {}

export const CasesScreen: React.FunctionComponent<CasesScreenProps> = (
  props
) => {
  const [error, setError] = React.useState<string | null>(null);
  const [caseSummaries, setCaseSummaries] = React.useState<Array<
    CaseSummary
  > | null>(null);

  const classes = useStyles();

  React.useEffect(() => {
    const fetchData = async () => {
      try {
        await casesSummarise(
          (caseSumaries) => {
            setCaseSummaries(caseSumaries);
          },
          (err: ClientError) => {
            setError(renderClientError(err));
          }
        );
      } catch (error) {
        setError(fallBackErrorMsg);
      }
    };
    fetchData();
  }, []);

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        <Grid item xs={12}>
          <Paper className={classes.contentPaper}>
            <Title>Cases</Title>
            {error && (
              <Alert severity="error">Couldn't get cases: {error}</Alert>
            )}

            {caseSummaries === null ? (
              error ? null : (
                <CircularProgress />
              )
            ) : (
              <CaseSummaries caseSummaries={caseSummaries} />
            )}
          </Paper>
        </Grid>

        <Grid item xs={12}>
          <Paper className={classes.contentPaper}>
            <Title>New case</Title>
            <NewCaseForm />
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
