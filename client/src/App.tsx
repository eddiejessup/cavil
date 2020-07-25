import React from "react";
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Redirect,
} from "react-router-dom";
import {
  CssBaseline,
  AppBar,
  Toolbar,
  IconButton,
  Typography,
  Drawer,
  Divider,
  List,
  Hidden,
} from "@material-ui/core";
import MenuIcon from "@material-ui/icons/Menu";
import FolderIcon from "@material-ui/icons/Folder";
import AccountBoxIcon from "@material-ui/icons/AccountBox";
import { useStyles } from "./Components/Style";
import { CasesScreen } from "./Components/Case/CasesScreen";
import { CaseScreen } from "./Components/Case/CaseScreen";
import { LoginScreen } from "./Components/Login/LoginScreen";
import { ListItemLink } from "./Components/ListItemLink";

export const App: React.FunctionComponent<{}> = (props) => {
  const [mobileDrawerOpen, setMobileDrawerOpen] = React.useState(false);

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
        <ListItemLink
          to="/cases"
          key="cases"
          icon={<FolderIcon />}
          primary="Cases"
        />
        <ListItemLink
          to="/login"
          key="login"
          icon={<AccountBoxIcon />}
          primary="Log in"
        />
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

  return (
    <Router>
      <div className={classes.root}>
        <CssBaseline />
        {appBar}
        {navDrawer}
        <main className={classes.content}>
          <div className={classes.toolbar} />
          <Switch>
            <Route exact path="/">
              <Redirect to="/cases" />
            </Route>
            <Route exact path="/cases">
              <CasesScreen />
            </Route>
            <Route path="/cases/:caseId">
              <CaseScreen />
            </Route>
            <Route path="/login">
              <LoginScreen />
            </Route>
          </Switch>
        </main>
      </div>
    </Router>
  );
};
