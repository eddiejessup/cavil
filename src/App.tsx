import React from 'react'
import {makeStyles, useTheme, Theme, createStyles, CssBaseline, AppBar, Toolbar, IconButton, Typography, Drawer, Divider, List, ListItem, ListItemIcon, ListItemText, Hidden, Container, Grid, Paper, Table, TableHead, TableRow, TableCell, TableBody, FormControl, InputLabel, Select, MenuItem, CircularProgress} from '@material-ui/core'
import MenuIcon from '@material-ui/icons/Menu'
import FolderIcon from '@material-ui/icons/Folder'
import AssignmentIcon from '@material-ui/icons/Assignment';

// API.

type DecisionToken = string

interface CaseSummary {
  nextDecisionToken: DecisionToken
  label: string
  nrVariants: number
  decisions: Array<DecisionSummary>
}

interface DecisionSummary {
  token: DecisionToken
  decisionTimeUTC: string
  variant: number
}

// /API.

const drawerWidth = 240

type Page = 'cases' | 'decisions'

interface PageData {
  page: Page
  label: string
  icon: JSX.Element
}

const pageData: Array<PageData> = [
  {
    page: 'cases',
    label: 'Cases',
    icon: <FolderIcon />,
  },
  {
    page: 'decisions',
    label: 'Decisions',
    icon: <AssignmentIcon />,
  },
]

const Title: React.FunctionComponent<{}> = (props) => (
  <Typography component="h2" variant="h6" color="primary" gutterBottom>
    {props.children}
  </Typography>
)

const useStyles = makeStyles((theme: Theme) =>
  createStyles({
    root: {
      display: 'flex',
    },
    drawer: {
      [theme.breakpoints.up('sm')]: {
        width: drawerWidth,
        flexShrink: 0,
      },
    },
    appBar: {
      [theme.breakpoints.up('sm')]: {
        width: `calc(100% - ${drawerWidth}px)`,
        marginLeft: drawerWidth,
      },
    },
    menuButton: {
      marginRight: theme.spacing(2),
      [theme.breakpoints.up('sm')]: {
        display: 'none',
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
      display: 'flex',
      overflow: 'auto',
      flexDirection: 'column',
    },
    formControl: {
      margin: theme.spacing(1),
      minWidth: 120,
    },
  }),
)

const App: React.FunctionComponent<{}> = (props) => {
  const [mobileDrawerOpen, setMobileDrawerOpen] = React.useState(false)
  const [currentPage, setCurrentPage] = React.useState<Page>('cases')

  const handleMobileDrawerToggle = () => {
    setMobileDrawerOpen(!mobileDrawerOpen)
  }

  const classes = useStyles()
  const theme = useTheme()

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
  )

  const drawer = (
    <div>
      <div className={classes.toolbar} />
      <Divider />
      <List>
        {pageData.map(({page, label, icon}) => (
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
  )

  const navDrawer = (
    <nav className={classes.drawer} aria-label="pages">
      <Hidden smUp implementation="css">
        <Drawer
          variant="temporary"
          anchor={theme.direction === 'rtl' ? 'right' : 'left'}
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
  )

  let page
  if (currentPage === 'cases') {
    page = <Cases />
  } else if (currentPage === 'decisions') {
    page = <Decisions />
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
  )
}

const Cases: React.FunctionComponent<{}> = (props) => {
  const [error, setError] = React.useState<Error | null>(null)
  const [caseSummaries, setCaseSummaries] = React.useState<Array<CaseSummary> | null>(null)

  const classes = useStyles()

  React.useEffect(() => {
    const fetchData = async () => {
      if (caseSummaries == null) {
        setError(null)
        try {
          const res = await fetch("/case")
          const resJson = await res.json()
          setCaseSummaries(resJson)
        } catch (error) {
          setError(error)
        }
      }
    }
    fetchData()
  }, [caseSummaries])

  let content
  if (error) {
    content = (
      <p>Error: {error.message}</p>
    )
  } else if (caseSummaries == null) {
    content = (
      <CircularProgress />
    )
  } else {
    content = (
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
          {caseSummaries.map((caseSummary) => {
            const decisions = caseSummary.decisions
            const nrDecisions = decisions.length
            const lastDecision = decisions[nrDecisions - 1]
            return (
              <TableRow key={caseSummary.label}>
                <TableCell>{caseSummary.label}</TableCell>
                <TableCell>{caseSummary.nrVariants}</TableCell>
                <TableCell>{nrDecisions}</TableCell>
                <TableCell>{lastDecision ? lastDecision.decisionTimeUTC : ''}</TableCell>
                <TableCell>{lastDecision ? lastDecision.variant : ''}</TableCell>
              </TableRow>
            )})}
        </TableBody>
      </Table>
    )
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
  )
}

const Decisions: React.FunctionComponent<{}> = (props) => {
  const [caseLabelsError, setCaseLabelsError] = React.useState<Error | null>(null)
  const [caseLabels, setCaseLabels] = React.useState<Array<string> | null>(null)

  const [caseSummaryError, setCaseSummaryError] = React.useState<Error | null>(null)
  const [caseSummary, setCaseSummary] = React.useState<CaseSummary | null>(null)

  const [selectedCaseLabel, setCaseLabel] = React.useState<string | null>(null)

  const classes = useStyles()

  const handleSelectCase = (event: React.ChangeEvent<{ value: unknown }>) => {
    setCaseLabel(event.target.value as string);
  };


  React.useEffect(() => {
    const fetchData = async () => {
      if (caseLabels == null) {
        // Fetch labels.
        setCaseLabelsError(null)
        try {
          const res = await fetch("/case")
          const resJson = await res.json()
          setCaseLabels(resJson.map((caseSummary: CaseSummary) => caseSummary.label))
        } catch (error) {
          setCaseLabelsError(error)
        }
      } else if (selectedCaseLabel != null) {
        // Fetch summary for selected case.
        setCaseSummary(null)
        setCaseSummaryError(null)

        try {
          const res = await fetch(`/case/${selectedCaseLabel}`)
          const resJson = await res.json()
          setCaseSummary(resJson)
        } catch (error) {
          setCaseSummaryError(error)
        }
      } else {
        setCaseSummary(null)
        setCaseSummaryError(null)
      }
    }
    fetchData()
  }, [caseLabels, selectedCaseLabel])

  let caseSelectorElem
  if (caseLabelsError) {
    caseSelectorElem = (
      <p>Error loading case labels: {caseLabelsError.message}</p>
    )
  } else if (caseLabels == null) {
    caseSelectorElem = (
      <CircularProgress />
    )
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
          {caseLabels.map(caseLabel => (
            <MenuItem value={caseLabel}>{caseLabel}</MenuItem>
          ))}
        </Select>
      </FormControl>
    )
  }

  let caseSummaryElem
  if (selectedCaseLabel == null) {
    caseSummaryElem = null
  } else {
    let summaryContent
    if (caseSummaryError) {
      summaryContent = (
        <p>Error loading case summary: {caseSummaryError.message}</p>
      )
    } else if (caseSummary == null) {
      summaryContent = (
        <CircularProgress />
      )
    } else {
      summaryContent = (
        <React.Fragment>
          <p>Number of variants: {caseSummary.nrVariants}</p>

            <Table size="small">
              <TableHead>
                <TableRow>
                  <TableCell>Decision time (UTC)</TableCell>
                  <TableCell>Variant</TableCell>
                  <TableCell>Token</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {caseSummary.decisions.map(decisionSummary => (
                  <TableRow key={decisionSummary.token}>
                    <TableCell>{decisionSummary.decisionTimeUTC}</TableCell>
                    <TableCell>{decisionSummary.variant}</TableCell>
                    <TableCell>{decisionSummary.token}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
        </React.Fragment>
      )
    }

    caseSummaryElem = (
      <Grid item xs={12}>
        <Paper className={classes.contentPaper}>
          <Title>Case {selectedCaseLabel}</Title>
          {summaryContent}
        </Paper>
      </Grid>
    )
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

        {caseSummaryElem}
      </Grid>
    </Container>
  )
}

export default App
