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
import FolderIcon from "@material-ui/icons/Folder";
import {CaseLabel, CaseSummary, DecisionSummary} from "./Api"
import {NewCaseForm} from "./NewCase"
import {useStyles} from "./Style"
import {Title} from "./Common"
import {
  Link as RouterLink,
  useRouteMatch,
  useParams,
} from "react-router-dom";

export type Screen = "cases" | "decisions";

interface ScreenData {
  screen: Screen;
  label: CaseLabel;
  icon: JSX.Element;
  path: string;
}

export const screenData: Array<ScreenData> = [
  {
    screen: "cases",
    label: "Cases",
    icon: <FolderIcon />,
    path: 'cases'
  },
];

interface CasesScreenProps {
}

export const CasesScreen: React.FunctionComponent<CasesScreenProps> = (props) => {
  const [error, setError] = React.useState<Error | null>(null);
  const [caseSummaries, setCaseSummaries] = React.useState<Array<
    CaseSummary
  > | null>(null);

  const classes = useStyles();

  React.useEffect(() => {
    const fetchData = async () => {
      if (caseSummaries == null) {
        setError(null);
        try {
          const res = await fetch("/case");
          const resJson = await res.json();
          setCaseSummaries(resJson);
        } catch (error) {
          setError(error);
        }
      }
    };
    fetchData();
  }, [caseSummaries]);

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        <Grid item xs={12}>
          <Paper className={classes.contentPaper}>
            <Title>Cases</Title>
            {error && (
              <Alert severity="error">Could not get cases: {error.message}</Alert>
            )}

            {caseSummaries == null ?
              <CircularProgress /> :
              <CaseSummaries
                caseSummaries={caseSummaries}
              />
            }

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
  const {url} = useRouteMatch();

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

interface CaseScreenProps {
}

export const CaseScreen: React.FunctionComponent<CaseScreenProps> = (
  props
) => {
  const { caseLabel } = useParams();
  const classes = useStyles();

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        {caseLabel && (
          <Grid item xs={12}>
            <Paper className={classes.contentPaper}>
              <FetchedCaseSummary caseLabel={caseLabel} />
            </Paper>
          </Grid>
        )}
      </Grid>
    </Container>
  );
};

interface FetchedCaseSummaryProps {
  caseLabel: CaseLabel;
}

const FetchedCaseSummary: React.FunctionComponent<FetchedCaseSummaryProps> = (
  props
) => {
  const [error, setError] = React.useState<Error | null>(null);
  const [caseSummary, setCaseSummary] = React.useState<CaseSummary | null>(
    null
  );

  React.useEffect(() => {
    const fetchData = async () => {
      setCaseSummary(null);
      setError(null);

      try {
        const res = await fetch(`/case/${props.caseLabel}`);
        const resJson = await res.json();
        setCaseSummary(resJson);
      } catch (error) {
        setError(error);
      }
    };
    fetchData();
  }, [props.caseLabel]);

  return (
    <React.Fragment>
      <Title>Case {props.caseLabel}</Title>

      {error && (
        <Alert severity="error">
          Could not get case summary: {error.message}
        </Alert>
      )}

      {caseSummary == null ?
        <CircularProgress /> :
        (<CaseSummaryComponent caseSummary={caseSummary} />)
      }
    </React.Fragment>
  );
};

interface CaseSummaryProps {
  caseSummary: CaseSummary;
}

const CaseSummaryComponent: React.FunctionComponent<CaseSummaryProps> = (props) => {
  return (
    <React.Fragment>
      <p>Number of variants: {props.caseSummary.nrVariants}</p>

      <Table size="small">
        <TableHead>
          <TableRow>
            <TableCell>Decision time (UTC)</TableCell>
            <TableCell>Variant</TableCell>
            <TableCell>Token</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {props.caseSummary.decisions.map((decisionSummary: DecisionSummary) => (
            <TableRow key={decisionSummary.token}>
              <TableCell>{decisionSummary.decisionTimeUTC}</TableCell>
              <TableCell>{decisionSummary.variant}</TableCell>
              <TableCell>{decisionSummary.token}</TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </React.Fragment>
  );
};
