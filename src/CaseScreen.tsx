import React from "react";
import { Container, Grid, Chip } from "@material-ui/core";
import { Alert } from "@material-ui/lab";
import FormatListNumberedIcon from "@material-ui/icons/FormatListNumbered";
import {
  CaseSummary,
  ClientError,
  renderClientError,
  fallBackErrorMsg,
  caseSummarise,
} from "./Api";
import { useStyles } from "./Style";
import { Title } from "./Common";
import { NewDecision } from "./NewDecision";
import { DecisionList } from "./DecisionList";
import { useParams } from "react-router-dom";

interface CaseScreenProps {}

export const CaseScreen: React.FunctionComponent<CaseScreenProps> = (props) => {
  const { caseLabel } = useParams();
  const classes = useStyles();

  const [error, setError] = React.useState<string | null>(null);
  const [caseSummary, setCaseSummary] = React.useState<CaseSummary | null>(
    null
  );

  React.useEffect(() => {
    const fetchData = async () => {
      setCaseSummary(null);
      setError(null);

      try {
        await caseSummarise(
          caseLabel,
          setCaseSummary,
          (errObj: ClientError) => {
            setError(renderClientError(errObj));
          }
        );
      } catch (error) {
        setError(fallBackErrorMsg);
      }
    };

    fetchData();
  }, [caseLabel]);

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        {caseLabel && (
          <Grid item xs={12}>
            <Grid container spacing={1}>
              <Grid item>
                <Title>Case {caseLabel}</Title>
              </Grid>
              {caseSummary && (
                <Grid item>
                  <Chip
                    icon={<FormatListNumberedIcon />}
                    label={`${caseSummary.nrVariants} variants`}
                    color="primary"
                    size="small"
                  />
                </Grid>
              )}
            </Grid>
          </Grid>
        )}

        {error && (
          <Grid item xs={12}>
            <Alert severity="error">Couldn't get case summary: {error}</Alert>
          </Grid>
        )}

        <Grid item xs={12}>
          <NewDecision
            caseSummary={caseSummary === null ? undefined : caseSummary}
          />
        </Grid>

        <Grid item xs={12}>
          <DecisionList
            caseSummary={caseSummary === null ? undefined : caseSummary}
          />
        </Grid>
      </Grid>
    </Container>
  );
};
