package db

import (
	"database/sql"
	"encoding/json"
	"fmt"

	"github.com/concourse/atc"
)

func (db *SQLDB) CreateDefaultTeamIfNotExists() error {
	_, err := db.conn.Exec(`
	INSERT INTO teams (
    name, admin
	)
	SELECT $1, true
	WHERE NOT EXISTS (
		SELECT id FROM teams WHERE name = $1
	)
	`, atc.DefaultTeamName)
	if err != nil {
		return err
	}

	_, err = db.conn.Exec(`
		UPDATE teams
		SET admin = true
		WHERE name = $1
	`, atc.DefaultTeamName)
	return err
}

func (db *SQLDB) CreateTeam(team Team) (SavedTeam, error) {
	jsonEncodedBasicAuth, err := team.BasicAuth.EncryptedJSON()
	if err != nil {
		return SavedTeam{}, err
	}

	gitHubAuth := GitHubAuth{}
	if team.ClientID != "" && team.ClientSecret != "" {
		gitHubAuth = team.GitHubAuth
	}
	jsonEncodedGitHubAuth, err := json.Marshal(gitHubAuth)
	if err != nil {
		return SavedTeam{}, err
	}

	return db.queryTeam(fmt.Sprintf(`
	INSERT INTO teams (
    name, basic_auth, github_auth
	) VALUES (
		'%s', '%s', '%s'
	)
	RETURNING id, name, admin, basic_auth, github_auth
	`, team.Name, jsonEncodedBasicAuth, string(jsonEncodedGitHubAuth),
	))
}

func (db *SQLDB) queryTeam(query string) (SavedTeam, error) {
	var basicAuth, gitHubAuth sql.NullString
	var savedTeam SavedTeam

	tx, err := db.conn.Begin()
	if err != nil {
		return SavedTeam{}, err
	}
	defer tx.Rollback()

	err = tx.QueryRow(query).Scan(
		&savedTeam.ID,
		&savedTeam.Name,
		&savedTeam.Admin,
		&basicAuth,
		&gitHubAuth,
	)
	if err != nil {
		return savedTeam, err
	}
	err = tx.Commit()
	if err != nil {
		return savedTeam, err
	}

	if basicAuth.Valid {
		err = json.Unmarshal([]byte(basicAuth.String), &savedTeam.BasicAuth)
		if err != nil {
			return savedTeam, err
		}
	}

	if gitHubAuth.Valid {
		err = json.Unmarshal([]byte(gitHubAuth.String), &savedTeam.GitHubAuth)
		if err != nil {
			return savedTeam, err
		}
	}

	return savedTeam, nil
}

func (db *SQLDB) DeleteTeamByName(teamName string) error {
	_, err := db.conn.Exec(`
    DELETE FROM teams
		WHERE name ILIKE $1
	`, teamName)
	return err
}
