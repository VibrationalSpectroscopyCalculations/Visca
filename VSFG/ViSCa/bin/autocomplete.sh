#!/bin/bash
################################################################################
#                                                                              #
# Shell-script wrapper allowing autocomplete for ViSCa 2024                    #
#                                                                              #
# Script made by Kris Strunge                                                  #
#                                                                              #
#                                                                              #
################################################################################
# Source this file to allow auto completion off methods for visca
# 
# source "This/path/autocomplete.sh" >> ~/.bashrc
#
validmethods="select select_RSS_plot select_RSS_cutoff select_plotter select_shaded_plot select_visualize_ensemble orient orient_RSS_plot orient_RSS_cutoff orient_plotter orient_shaded_plot orient_visualize_ensemble"
complete -F _longopt -W '$validmethods' visca

