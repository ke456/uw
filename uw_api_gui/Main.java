package uw_api_gui;

import UW_API.CourseFinder;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class Main {
    public static void main (String []args){
        System.out.println(CourseFinder.getTestSchedule(CourseFinder.getDetails("CS","135")).apply(0).apply(0));
    }
}
