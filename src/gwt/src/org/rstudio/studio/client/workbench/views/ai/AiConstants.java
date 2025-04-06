/*
 * AiConstants.java
 *
 * Copyright (C) 2022 by Posit Software, PBC
 *
 * Unless you have received this program directly from Posit Software pursuant
 * to the terms of a commercial license agreement with Posit Software, then
 * this program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
 *
 */
package org.rstudio.studio.client.workbench.views.ai;

public interface AiConstants extends com.google.gwt.i18n.client.Messages {

    /**
     * Translated "Ai".
     *
     * @return translated "Ai"
     */
    @DefaultMessage("Ai")
    @Key("aiText")
    String aiText();

    /**
     * Translated "Ai Pane".
     *
     * @return translated "Ai Pane"
     */
    @DefaultMessage("Ai Pane")
    @Key("aiPaneTitle")
    String aiPaneTitle();

    /**
     * Translated "Ai Tab".
     *
     * @return translated "Ai Tab"
     */
    @DefaultMessage("Ai Tab")
    @Key("aiTabLabel")
    String aiTabLabel();

    /**
     * Translated "Ai Tab Second".
     *
     * @return translated "Ai Tab Second"
     */
    @DefaultMessage("Ai Tab Second")
    @Key("aiTabSecondLabel")
    String aiTabSecondLabel();

    /**
     * Translated "Find next (Enter)".
     *
     * @return translated "Find next (Enter)"
     */
    @DefaultMessage("Find next (Enter)")
    @Key("findNextLabel")
    String findNextLabel();

    /**
     * Translated "Find previous".
     *
     * @return translated "Find previous"
     */
    @DefaultMessage("Find previous")
    @Key("findPreviousLabel")
    String findPreviousLabel();

    /**
     * Translated "Find in Topic".
     *
     * @return translated "Find in Topic"
     */
    @DefaultMessage("Find in Topic")
    @Key("findInTopicLabel")
    String findInTopicLabel();

    /**
     * Translated "No occurrences found".
     *
     * @return translated "No occurrences found"
     */
    @DefaultMessage("No occurrences found")
    @Key("noOccurrencesFoundMessage")
    String noOccurrencesFoundMessage();

    /**
     * Translated "Search ai".
     *
     * @return translated "Search ai"
     */
    @DefaultMessage("Search ai")
    @Key("searchAiLabel")
    String searchAiLabel();

}
