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
  Chip,
  Typography,
} from "@material-ui/core";
import { Alert } from "@material-ui/lab";
import CategoryIcon from "@material-ui/icons/Category";
import { CaseLabel, CaseSummary, DecisionSummary } from "./Api";
import { useStyles } from "./Style";
import { Title, SubTitle } from "./Common";
import { useParams } from "react-router-dom";

interface CaseScreenProps {}

export const CaseScreen: React.FunctionComponent<CaseScreenProps> = (props) => {
  const { caseLabel } = useParams();
  const classes = useStyles();

  const [error, setError] = React.useState<Error | null>(null);
  const [caseSummary, setCaseSummary] = React.useState<CaseSummary | null>(
    null
  );

  React.useEffect(() => {
    const fetchData = async () => {
      setCaseSummary(null);
      setError(null);

      try {
        const res = await fetch(`/case/${caseLabel}`);
        const resJson = await res.json();
        setCaseSummary(resJson);
      } catch (error) {
        setError(error);
      }
    };
    fetchData();
  }, [caseLabel]);

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        {caseLabel && (
          <Grid item xs={12}>
            <Title>
              <Grid container spacing={1}>
                <Grid item>Case {caseLabel}</Grid>
                {caseSummary && (
                  <Grid item>
                    <Chip
                      icon={<CategoryIcon />}
                      label={`${caseSummary.nrVariants} variants`}
                      color="primary"
                      size="small"
                    />
                  </Grid>
                )}
              </Grid>
            </Title>
          </Grid>
        )}

        {error && (
          <Grid item xs={12}>
            <Alert severity="error">
              Could not get case summary: {error.message}
            </Alert>
          </Grid>
        )}

        <Grid item xs={12}>
          <DecisionList
            caseSummary={caseSummary === null ? undefined : caseSummary}
          />
        </Grid>
      </Grid>
    </Container>
  );
};

interface DecisionListProps {
  caseSummary?: CaseSummary;
}

const DecisionList: React.FunctionComponent<DecisionListProps> = (props) => {
  const classes = useStyles();

  const content =
    props.caseSummary === undefined ? (
      <CircularProgress />
    ) : props.caseSummary.decisions.length === 0 ? (
      <Alert severity="info">No decisions yet</Alert>
    ) : (
      <Table size="small">
        <TableHead>
          <TableRow>
            <TableCell>Decision time (UTC)</TableCell>
            <TableCell>Variant</TableCell>
            <TableCell>Token</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {props.caseSummary.decisions.map(
            (decisionSummary: DecisionSummary) => (
              <TableRow key={decisionSummary.token}>
                <TableCell>{decisionSummary.decisionTimeUTC}</TableCell>
                <TableCell>{decisionSummary.variant}</TableCell>
                <TableCell>{decisionSummary.token}</TableCell>
              </TableRow>
            )
          )}
        </TableBody>
      </Table>
    );

  return (
    <Paper className={classes.contentPaper}>
      <Grid container spacing={2} direction="column">
        <Grid item>
          <SubTitle>Decisions</SubTitle>
        </Grid>

        <Grid item>{content}</Grid>
      </Grid>
    </Paper>
  );
};

interface NewDecisionProps {
  caseSummary: CaseSummary;
}

const NewDecision: React.FunctionComponent<NewDecisionProps> = (props) => {
  const classes = useStyles();
  return (
    <Paper className={classes.contentPaper}>
      <Grid container spacing={2} direction="column">
        <Grid item>
          <SubTitle>New decision</SubTitle>
        </Grid>

        <Grid item>
          <Alert severity="info">
            Decision token: {props.caseSummary.nextDecisionToken}
          </Alert>
        </Grid>
      </Grid>
    </Paper>
  );
};
