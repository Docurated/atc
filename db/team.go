package db

import (
	"encoding/json"

	"golang.org/x/crypto/bcrypt"
)

type Team struct {
	Name  string
	Admin bool
	BasicAuth
	GitHubAuth
}

type BasicAuth struct {
	BasicAuthUsername string `json:"basic_auth_username"`
	BasicAuthPassword string `json:"basic_auth_password"`
}

func (auth *BasicAuth) EncryptedJSON() (string, error) {
	result := BasicAuth{}
	if auth.BasicAuthUsername != "" && auth.BasicAuthPassword != "" {
		encryptedPw, err := bcrypt.GenerateFromPassword([]byte(auth.BasicAuthPassword), 4)
		if err != nil {
			return "", err
		}
		result.BasicAuthPassword = string(encryptedPw)
		result.BasicAuthUsername = auth.BasicAuthUsername
	}

	json, err := json.Marshal(result)
	return string(json), err
}

type GitHubAuth struct {
	ClientID      string       `json:"client_id"`
	ClientSecret  string       `json:"client_secret"`
	Organizations []string     `json:"organizations"`
	Teams         []GitHubTeam `json:"teams"`
	Users         []string     `json:"users"`
	AuthURL       string       `json:"authurl"`
	TokenURL      string       `json:"tokenurl"`
	APIURL        string       `json:"apiurl"`
}

type GitHubTeam struct {
	OrganizationName string `json:"organization_name"`
	TeamName         string `json:"team_name"`
}

type SavedTeam struct {
	ID int
	Team
}
