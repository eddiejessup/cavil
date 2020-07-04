import React from "react";
import {
  Grid,
  Paper,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  CircularProgress,
  Button,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  TextField,
} from "@material-ui/core";
import { Alert } from "@material-ui/lab";
import {
  CaseSummary,
  DecisionToken,
  DecisionSummary,
  ClientError,
  renderClientError,
  fallBackErrorMsg,
  caseDecisionInvalidate,
} from "./Api";
import { useStyles } from "./Style";
import { SubTitle, FetchState } from "./Common";

export interface DecisionListProps {
  fetchedCaseSummary: FetchState<CaseSummary>;
  onCaseChanged: () => void;
}

export const DecisionList: React.FunctionComponent<DecisionListProps> = ({
  fetchedCaseSummary,
  onCaseChanged,
}) => {
  const classes = useStyles();

  const [
    tokenToConfirmInvalidation,
    setTokenToConfirmInvalidation,
  ] = React.useState<DecisionToken | null>(null);
  const [invalidationError, setInvalidationError] = React.useState<
    string | null
  >(null);

  const handleInvalidateToken = async (reason: string) => {
    // tokenToConfirmInvalidation shouldn’t be null because dialog is open iff
    // it's non-null, but want to prove that to typescript.
    // caseSummary shouldn't be undefined because we show the table iff
    // it's defined, but want to prove that to typescript.
    if (
      tokenToConfirmInvalidation !== null &&
      fetchedCaseSummary.kind === "fetchSuccess"
    ) {
      setInvalidationError(null);
      await caseDecisionInvalidate(
        fetchedCaseSummary.value.label,
        tokenToConfirmInvalidation,
        reason,
        () => {
          setTokenToConfirmInvalidation(null);
          onCaseChanged();
        },
        (err: ClientError) => {
          setInvalidationError(renderClientError(err));
        },
        (_err: Error) => {
          setInvalidationError(fallBackErrorMsg);
        }
      );
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
      content =
        fetchedCaseSummary.value.decisions.length === 0 ? (
          <Alert severity="info">No decisions yet</Alert>
        ) : (
          <React.Fragment>
            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Decision time (UTC)</TableCell>
                  <TableCell>Variant</TableCell>
                  <TableCell>Token</TableCell>
                  <TableCell></TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {fetchedCaseSummary.value.decisions.map(
                  (decisionSummary: DecisionSummary) => (
                    <TableRow
                      key={decisionSummary.token}
                      className={decisionSummary.isValid ? "" : classes.invalid}
                    >
                      <TableCell>{decisionSummary.decisionTimeUTC}</TableCell>
                      <TableCell>{decisionSummary.variant}</TableCell>
                      <TableCell>{decisionSummary.token}</TableCell>
                      <TableCell>
                        {decisionSummary.isValid ? (
                          <Button
                            variant="outlined"
                            color="secondary"
                            size="small"
                            onClick={() => {
                              setTokenToConfirmInvalidation(
                                decisionSummary.token
                              );
                            }}
                          >
                            Invalidate
                          </Button>
                        ) : null}
                      </TableCell>
                    </TableRow>
                  )
                )}
              </TableBody>
            </Table>

            <InvalidateDialog
              classes={{
                paper: classes.contentPaper,
              }}
              id="invalidate-dialog"
              keepMounted
              open={tokenToConfirmInvalidation !== null}
              onClose={(reason?: string) => {
                // reason is undefined iff we cancelled.
                if (reason === undefined) {
                  setTokenToConfirmInvalidation(null);
                  setInvalidationError(null);
                } else {
                  handleInvalidateToken(reason);
                }
              }}
              error={invalidationError === null ? undefined : invalidationError}
            />
          </React.Fragment>
        );
      break;
  }

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

interface InvalidateDialogProps {
  classes: Record<"paper", string>;
  id: string;
  keepMounted: boolean;
  open: boolean;
  onClose: (reason?: string) => void;
  error?: string;
}

const InvalidateDialog: React.FunctionComponent<InvalidateDialogProps> = (
  props
) => {
  const { onClose, open, error, ...other } = props;
  const [reason, setReason] = React.useState("");
  const reasonFieldRef = React.useRef<HTMLElement>(null);

  React.useEffect(() => {
    if (!open) {
      setReason("");
    }
  }, [open]);

  const handleEntering = () => {
    if (reasonFieldRef.current !== null) {
      reasonFieldRef.current.focus();
    }
  };

  const handleCancel = () => {
    onClose();
  };

  const handleOk = () => {
    onClose(reason);
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setReason(event.target.value);
  };

  return (
    <Dialog
      onEntering={handleEntering}
      aria-labelledby="confirmation-dialog-title"
      open={open}
      {...other}
    >
      <DialogTitle id="confirmation-dialog-title">
        Invalidate decision
      </DialogTitle>

      <DialogContent>
        <Grid container direction="column" spacing={3}>
          <Grid item>
            <TextField
              inputRef={reasonFieldRef}
              id="reason"
              label="Reason"
              variant="outlined"
              required
              value={reason}
              onChange={handleChange}
            />
          </Grid>

          {error && (
            <Grid item>
              <Alert severity="error">{error}</Alert>
            </Grid>
          )}
        </Grid>
      </DialogContent>
      <DialogActions>
        <Button autoFocus onClick={handleCancel} color="primary">
          Cancel
        </Button>
        <Button
          onClick={handleOk}
          color="primary"
          disabled={reason.trim() === ""}
        >
          Confirm
        </Button>
      </DialogActions>
    </Dialog>
  );
};
