import React from "react";
import FolderIcon from "@material-ui/icons/Folder";
import { CaseLabel } from "./Api";

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
    path: "cases",
  },
];
