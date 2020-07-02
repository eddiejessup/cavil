import React from "react";
import {
  makeStyles,
  useTheme,
  Theme,
  createStyles,
  CssBaseline,
  AppBar,
  Toolbar,
  IconButton,
  Typography,
  Drawer,
  Divider,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
  Hidden,
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
import MenuIcon from "@material-ui/icons/Menu";
import FolderIcon from "@material-ui/icons/Folder";
import AssignmentIcon from "@material-ui/icons/Assignment";
import {CaseLabel, CaseSummary, DecisionSummary} from "./Api"

const drawerWidth = 240;

type Page = "cases" | "decisions";

interface PageData {
  page: Page;
  label: CaseLabel;
  icon: JSX.Element;
}

const pageData: Array<PageData> = [
  {
    page: "cases",
    label: "Cases",
    icon: <FolderIcon />,
  },
  {
    page: "decisions",
    label: "Decisions",
    icon: <AssignmentIcon />,
  },
];

const Title: React.FunctionComponent<{}> = (props) => (
  <Typography component="h2" variant="h6" color="primary" gutterBottom>
    {props.children}
  </Typography>
);

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    root: {
      display: "flex",
    },
    drawer: {
      [theme.breakpoints.up("sm")]: {
        width: drawerWidth,
        flexShrink: 0,
      },
    },
    appBar: {
      [theme.breakpoints.up("sm")]: {
        width: `calc(100% - ${drawerWidth}px)`,
        marginLeft: drawerWidth,
      },
    },
    menuButton: {
      marginRight: theme.spacing(2),
      [theme.breakpoints.up("sm")]: {
        display: "none",
      },
    },
    // Put content below app bar
    toolbar: theme.mixins.toolbar,
    drawerPaper: {
      width: drawerWidth,
    },
    content: {
      flexGrow: 1,
      padding: theme.spacing(3),
    },
    container: {
      paddingTop: theme.spacing(4),
      paddingBottom: theme.spacing(4),
    },
    contentPaper: {
      padding: theme.spacing(2),
      display: "flex",
      overflow: "auto",
      flexDirection: "column",
    },
    formControl: {
      margin: theme.spacing(1),
      minWidth: 120,
    },
  })
);

const App: React.FunctionComponent<{}> = (props) => {
  const [mobileDrawerOpen, setMobileDrawerOpen] = React.useState(false);
  const [currentPage, setCurrentPage] = React.useState<Page>("cases");
  const [currentCase, setCurrentCase] = React.useState<CaseLabel | null>(null);

  const handleMobileDrawerToggle = () => {
    setMobileDrawerOpen(!mobileDrawerOpen);
  };

  const classes = useStyles();
  const theme = useTheme();

  const appBar = (
    <AppBar position="fixed" className={classes.appBar}>
      <Toolbar>
        <IconButton
          color="inherit"
          aria-label="open drawer"
          edge="start"
          onClick={handleMobileDrawerToggle}
          className={classes.menuButton}
        >
          <MenuIcon />
        </IconButton>
        <Typography variant="h6" noWrap>
          Cavil
        </Typography>
      </Toolbar>
    </AppBar>
  );

  const drawer = (
    <div>
      <div className={classes.toolbar} />
      <Divider />
      <List>
        {pageData.map(({ page, label, icon }) => (
          <ListItem
            button
            key={page}
            selected={page === currentPage}
            onClick={() => setCurrentPage(page)}
          >
            <ListItemIcon>{icon}</ListItemIcon>
            <ListItemText primary={label} />
          </ListItem>
        ))}
      </List>
      <Divider />
    </div>
  );

  const navDrawer = (
    <nav className={classes.drawer} aria-label="pages">
      <Hidden smUp implementation="css">
        <Drawer
          variant="temporary"
          anchor={theme.direction === "rtl" ? "right" : "left"}
          open={mobileDrawerOpen}
          onClose={handleMobileDrawerToggle}
          classes={{
            paper: classes.drawerPaper,
          }}
          ModalProps={{
            keepMounted: true, // Better open performance on mobile.
          }}
        >
          {drawer}
        </Drawer>
      </Hidden>
      <Hidden xsDown implementation="css">
        <Drawer
          classes={{
            paper: classes.drawerPaper,
          }}
          variant="permanent"
          open
        >
          {drawer}
        </Drawer>
      </Hidden>
    </nav>
  );

  let page;
  switch (currentPage) {
    case "cases":
      page = (
        <CasesScreen
          onSelectCase={(caseLabel: CaseLabel) => {
            setCurrentCase(caseLabel);
            setCurrentPage("decisions");
          }}
        />
      );
      break
    case "decisions":
      page = (
        <DecisionsScreen
          currentCase={currentCase === null ? undefined : currentCase}
        />
      );
      break
    default:
      break
  }

  return (
    <div className={classes.root}>
      <CssBaseline />
      {appBar}
      {navDrawer}
      <main className={classes.content}>
        <div className={classes.toolbar} />
        {page}
      </main>
    </div>
  );
};

interface CasesScreenProps {
  onSelectCase: (caseLabel: CaseLabel) => void;
}

const CasesScreen: React.FunctionComponent<CasesScreenProps> = (props) => {
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

  let content;
  if (error) {
    content = (
      <Alert severity="error">Could not get cases: {error.message}</Alert>
    );
  } else if (caseSummaries == null) {
    content = <CircularProgress />;
  } else {
    content = (
      <CaseSummaries
        caseSummaries={caseSummaries}
        onSelectCase={props.onSelectCase}
      />
    );
  }

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        <Grid item xs={12}>
          <Paper className={classes.contentPaper}>
            <Title>Cases</Title>
            {content}
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

const DecisionsScreen: React.FunctionComponent<DecisionsScreenProps> = (
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

  let caseSelectorElem;
  if (caseLabelsError) {
    caseSelectorElem = (
      <Alert severity="error">
        Could not get case labels: {caseLabelsError.message}{" "}
      </Alert>
    );
  } else if (caseLabels == null) {
    caseSelectorElem = <CircularProgress />;
  } else {
    caseSelectorElem = (
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
    );
  }

  return (
    <Container maxWidth="lg" className={classes.container}>
      <Grid container spacing={3}>
        <Grid item xs={12}>
          <Paper className={classes.contentPaper}>
            <Title>Pick case</Title>
            {caseSelectorElem}
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

  let content;
  if (error) {
    content = (
      <Alert severity="error">
        Could not get case summary: {error.message}
      </Alert>
    );
  } else if (caseSummary == null) {
    content = <CircularProgress />;
  } else {
    content = <CaseSummaryComponent caseSummary={caseSummary} />;
  }

  return (
    <React.Fragment>
      <Title>Case {props.caseLabel}</Title>
      {content}
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

export default App;
