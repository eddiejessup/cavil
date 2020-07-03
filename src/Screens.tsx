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
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  CircularProgress,
  Link,
} from "@material-ui/core";
import { Alert } from "@material-ui/lab";
import FolderIcon from "@material-ui/icons/Folder";
import AssignmentIcon from "@material-ui/icons/Assignment";
import {CaseLabel, CaseSummary, DecisionSummary} from "./Api"
import {NewCaseForm} from "./NewCase"
import {useStyles} from "./Style"
import {Title} from "./Common"

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
  {
    screen: "decisions",
    label: "Decisions",
    icon: <AssignmentIcon />,
    path: 'decisions'
  },
];

interface CasesScreenProps {
  onSelectCase: (caseLabel: CaseLabel) => void;
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
                onSelectCase={props.onSelectCase}
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
  onSelectCase: (caseLabel: CaseLabel) => void;
}

const CaseSummaries: React.FunctionComponent<CaseSummariesProps> = (props) => {
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
                <Link
                  href="#"
                  onClick={() => {
                    props.onSelectCase(caseSummary.label);
                  }}
                >
                  {caseSummary.label}
                </Link>
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

interface DecisionsScreenProps {
  currentCase?: CaseLabel;
}

export const DecisionsScreen: React.FunctionComponent<DecisionsScreenProps> = (
  props
) => {
  const [caseLabelsError, setCaseLabelsError] = React.useState<Error | null>(
    null
  );
  const [caseLabels, setCaseLabels] = React.useState<Array<CaseLabel> | null>(
    null
  );
  const [selectedCaseLabel, setCaseLabel] = React.useState<CaseLabel | null>(
    props.currentCase ? props.currentCase : null
  );

  const classes = useStyles();

  const handleSelectCase = (event: React.ChangeEvent<{ value: unknown }>) => {
    setCaseLabel(event.target.value as CaseLabel);
  };

  React.useEffect(() => {
    const fetchData = async () => {
      setCaseLabelsError(null);
      try {
        const res = await fetch("/case");
        const resJson = await res.json();
        setCaseLabels(
          resJson.map((caseSummary: CaseSummary) => caseSummary.label)
        );
      } catch (error) {
        setCaseLabelsError(error);
      }
    };
    fetchData();
  }, []);

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        <Grid item xs={12}>
          <Paper className={classes.contentPaper}>
            <Title>Pick case</Title>

            {caseLabelsError && (
              <Alert severity="error">
                Could not get case labels: {caseLabelsError.message}{" "}
              </Alert>
            )}

            {caseLabels == null ?
              <CircularProgress /> : (
                <FormControl className={classes.formControl}>
                  <InputLabel id="decisions-case-label">Case</InputLabel>
                  <Select
                    labelId="decisions-case-label"
                    id="decisions-case"
                    value={selectedCaseLabel}
                    onChange={handleSelectCase}
                  >
                    {caseLabels.map((caseLabel) => (
                      <MenuItem value={caseLabel}>{caseLabel}</MenuItem>
                    ))}
                  </Select>
                </FormControl>
              )
            }
          </Paper>
        </Grid>

        {selectedCaseLabel && (
          <Grid item xs={12}>
            <Paper className={classes.contentPaper}>
              <FetchedCaseSummary caseLabel={selectedCaseLabel} />
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
