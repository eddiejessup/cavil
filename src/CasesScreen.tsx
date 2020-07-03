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
import { CaseSummary } from "./Api";
import { NewCaseForm } from "./NewCase";
import { useStyles } from "./Style";
import { Title } from "./Common";
import { Link as RouterLink, useRouteMatch } from "react-router-dom";

interface CasesScreenProps {}

export const CasesScreen: React.FunctionComponent<CasesScreenProps> = (
  props
) => {
  const [error, setError] = React.useState<Error | null>(null);
  const [caseSummaries, setCaseSummaries] = React.useState<Array<
    CaseSummary
  > | null>(null);

  const classes = useStyles();

  React.useEffect(() => {
    const fetchData = async () => {
      try {
        const res = await fetch("/case");
        const resJson = await res.json();
        setCaseSummaries(resJson);
      } catch (error) {
        setError(error);
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
              <Alert severity="error">
                Could not get cases: {error.message}
              </Alert>
            )}

            {caseSummaries === null ? (
              <CircularProgress />
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
          const decisions = caseSummary.decisions;
          const nrDecisions = decisions.length;
          const lastDecision = decisions[nrDecisions - 1];
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
              <TableCell>{nrDecisions}</TableCell>
              <TableCell>
                {lastDecision ? lastDecision.decisionTimeUTC : ""}
              </TableCell>
              <TableCell>{lastDecision ? lastDecision.variant : ""}</TableCell>
            </TableRow>
          );
        })}
      </TableBody>
    </Table>
  );
};
