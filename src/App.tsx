import React from "react";
import {
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
} from "@material-ui/core";
import MenuIcon from "@material-ui/icons/Menu";
import {CaseLabel} from "./Api"
import {useStyles} from "./Style"
import {Screen, screenData, CasesScreen, DecisionsScreen} from "./Screens"

export const App: React.FunctionComponent<{}> = (props) => {
  const [mobileDrawerOpen, setMobileDrawerOpen] = React.useState(false);
  const [currentScreen, setCurrentScreen] = React.useState<Screen>("cases");
  const [currentCase, setCurrentCase] = React.useState<CaseLabel | null>(null);

  const handleMobileDrawerToggle = () => {
    setMobileDrawerOpen(!mobileDrawerOpen);
  };

  const classes = useStyles();

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
        {screenData.map(({ screen, label, icon }) => (
          <ListItem
            button
            key={screen}
            selected={screen === currentScreen}
            onClick={() => setCurrentScreen(screen)}
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
    <nav className={classes.drawer} aria-label="screens">
      <Hidden smUp implementation="css">
        <Drawer
          variant="temporary"
          anchor="left"
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

  let screen;
  switch (currentScreen) {
    case "cases":
      screen = (
        <CasesScreen
          onSelectCase={(caseLabel: CaseLabel) => {
            setCurrentCase(caseLabel);
            setCurrentScreen("decisions");
          }}
        />
      );
      break
    case "decisions":
      screen = (
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
        {screen}
      </main>
    </div>
  );
};
