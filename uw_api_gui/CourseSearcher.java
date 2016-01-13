package uw_api_gui;

import UW_API.CourseFinder;

import javax.swing.*;
import java.awt.*;

public class CourseSearcher extends JPanel {
    JTextField h = new JTextField("Hello");

    CourseSearcher(){
        super.setBackground(Color.WHITE);
        super.add(h);
    }
}
